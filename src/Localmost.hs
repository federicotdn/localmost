module Localmost
  ( ast,
    check,
    validate,
    parseConfig,
    computePolicy,
    astAsScript,
    Runtime (..),
  )
where

import Config (Config (..), ConfigRule (..), configPath, loadConfig)
import Control.Monad (when)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.IntSet qualified as IntSet
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Protocol (Proto (..))
import Shell
  ( comments,
    fullCommands,
    isQuoted,
    literalText,
    parseShellScript,
    simpleCommands,
  )
import ShellCheck.AST qualified as AST
import ShellCheck.Interface (ParseResult (..))
import System.Exit (exitFailure)
import System.FilePath (isValid)
import Text.Parsec (Parsec)
import Text.Parsec qualified as P
import Text.Parsec.Error qualified as P
import Types
  ( Command (..),
    Count (..),
    Errors,
    Part (..),
    PipeAccess (..),
    Policy (..),
    Redirect (..),
    RedirectAccess (..),
    RedirectMode (..),
    Script (..),
    showToken,
  )
import Utils (ePutStrLn, isInt)

newtype Runtime = Runtime {rRules :: [Rule]} deriving (Show)

newtype Except = Except [Part] deriving (Show)

data Rule = Rule
  { rCommand :: Command,
    rPolicy :: Policy,
    rPipeAccess :: PipeAccess,
    rRedirectAccess :: RedirectAccess,
    rExcepts :: [Except]
  }
  deriving (Show)

data ParseRuleError = ParseRuleError
  { preErrors :: Errors,
    preComments :: [Text],
    preRule :: ConfigRule
  }

instance Show ParseRuleError where
  show pre =
    unpack $
      "In rule: \""
        <> rRule (preRule pre)
        <> "\""
        <> section "Error(s)" (preErrors pre)
        <> section "ShellCheck comments" (preComments pre)
    where
      section _ [] = ""
      section title items =
        "\n| " <> title <> ":" <> mconcat (map ("\n|   " <>) items)

parseRuleExcepts :: ConfigRule -> Either ParseRuleError [Except]
parseRuleExcepts r =
  let texts = fromMaybe [] (rUnless r)
   in mapM parseOne texts
  where
    parseOne t =
      let pr = parseShellScript t
          escript = astAsScript pr True
          err msgs = Left ParseRuleError {preErrors = msgs, preComments = [], preRule = r}
          anything = Quant Arg (Count 0 Nothing)
       in case escript of
            Right Script {sCommands = [c]} ->
              if cmdAllRedirectsCount c == 0
                then Right $ Except ([anything] ++ cmdParts c ++ [anything])
                else err ["Unless clauses must not contain redirections."]
            Right Script {sCommands = cmds} ->
              err ["Unless clauses must have exactly one command (got: " <> pack (show (length cmds)) <> ")."]
            Left errs -> err errs

parseRule :: Policy -> ConfigRule -> Either ParseRuleError Rule
parseRule pol r =
  let pr = parseShellScript (rRule r)
      escript = astAsScript pr True
      err msgs = Left ParseRuleError {preErrors = msgs, preComments = comments pr, preRule = r}
   in case escript of
        Right Script {sCommands = [c]} ->
          if cmdAllRedirectsCount c == 0
            then do
              excepts <- parseRuleExcepts r
              let pa = fromMaybe All (rPipe r) -- Allow all pipes by default.
                  ra = fromMaybe Safe (rRedirect r) -- Allow safe redirects by default.
              Right Rule {rCommand = c, rPolicy = pol, rPipeAccess = pa, rRedirectAccess = ra, rExcepts = excepts}
            else err ["Rules must not contain redirections."]
        Right Script {sCommands = cmds} ->
          err [pack $ "Rules must contain exactly one command (got: " ++ show (length cmds) ++ ")."]
        Left errs -> err errs

parseConfig :: Config -> Either [ParseRuleError] Runtime
parseConfig config =
  let parse p = partitionEithers . maybe [] (map (parseRule p))
      (allowErrs, pallow) = parse Allow (cAllow config)
      (denyErrs, pdeny) = parse Deny (cDeny config)
      allErrs = allowErrs ++ denyErrs
   in if null allErrs
        then
          Right Runtime {rRules = pdeny ++ pallow}
        else Left allErrs

astAsScript :: ParseResult -> Bool -> Either Errors Script
astAsScript pr isRule = case prRoot pr of
  Just root -> do
    let scommands = simpleCommands root
    let commands = fullCommands root
    if length scommands == length commands
      then do
        commands' <- mapM (`buildCommand` isRule) commands
        Right Script {sCommands = commands'}
      else Left [pack "Unexpected command placement in script."]
  Nothing -> Left [pack "Unable to parse script."]

unexpectedToken :: AST.Token -> Either Errors a
unexpectedToken t = Left [pack $ "Unexpected token: " ++ showToken t ++ "."]

buildCommand :: (AST.Token, AST.Token) -> Bool -> Either Errors Command
buildCommand (AST.T_Pipeline _ _ cmds, redir@(AST.T_Redirecting _ redirs cmd)) isRule = do
  let pipelineCmdIds = map AST.getId cmds
  let cmdId = AST.getId redir
  cmdIndex <- case elemIndex cmdId pipelineCmdIds of
    Just i -> Right i
    Nothing -> Left ["Command not found in pipeline."]
  parts <- simpleCommandParts cmd isRule
  let pipeIn = cmdIndex > 0
  let pipeOut = cmdIndex < length cmds - 1
  extractedRedirs <- extractRedirects redirs
  Right
    Command
      { cmdParts = parts,
        cmdPolicy = Nothing,
        cmdPipeIn = pipeIn,
        cmdPipeOut = pipeOut,
        cmdAllRedirectsCount = length redirs,
        cmdRedirects = extractedRedirs
      }
buildCommand (AST.T_Pipeline {}, t) _ = unexpectedToken t
buildCommand (t, _) _ = unexpectedToken t

extractRedirects :: [AST.Token] -> Either Errors [Redirect]
extractRedirects redirs =
  -- Only look at IoFile redirects.
  -- The other ones (IoDuplicate, HereDoc and HereString) can't read
  -- or write files, so they are not extracted.
  let ioRedirs = [(op, f) | AST.T_FdRedirect _ _ (AST.T_IoFile _ op f) <- redirs]
   in mapM extractOne ioRedirs
  where
    extractOne (op, f) = do
      mode <- case op of
        AST.T_Greater {} -> Right Write
        AST.T_DGREAT {} -> Right Write
        AST.T_CLOBBER {} -> Right Write
        AST.T_LESSGREAT {} -> Right Write
        AST.T_Less {} -> Right Read
        _ -> Left [pack $ "Unknown redirect operator: " ++ showToken op ++ "."]
      case literalText f False of
        Just path -> Right (StaticPath path mode)
        Nothing -> Right (DynamicPath f mode)

simpleCommandParts :: AST.Token -> Bool -> Either Errors [Part]
simpleCommandParts t isRule = case t of
  AST.T_SimpleCommand _ _ list ->
    if isRule
      then mapM asMetaPart list
      else Right (map Token list)
  _ -> unexpectedToken t

asMetaPart :: AST.Token -> Either Errors Part
asMetaPart t =
  -- Parse: @foo (not quoted!)
  let mtext = literalText t True
      hasPrefix = maybe False ("@" `T.isPrefixOf`) mtext
      -- Avoid parsing here single-element groups e.g. @{x}
      isChoice = maybe False ("@{" `T.isPrefixOf`) mtext
      expr = maybe "" (T.drop 1) mtext
      parsed = P.parse parseMetaExpr "" (T.unpack expr)
      quoted = isQuoted t
   in if hasPrefix && not quoted && not isChoice
        then case parsed of
          Right r -> Right r
          Left err ->
            let msgs = [pack s | P.Message s <- P.errorMessages err, not (null s)]
             in Left $ if null msgs then ["Unknown meta var: " <> fromMaybe "" mtext <> "."] else msgs
        else asMetaPartNonText t
  where
    asMetaPartNonText t' =
      case t of
        (AST.T_NormalWord _ list) -> case list of
          -- Parse: @{foo,bar}
          [AST.T_Literal _ "@", AST.T_BraceExpansion _ items] ->
            buildQuantifiedChoice items Nothing
          -- Parse: @{foo,bar}? / @{foo,bar}*
          [AST.T_Literal _ "@", AST.T_BraceExpansion _ items, AST.T_Glob _ s] ->
            buildQuantifiedChoice items $ Just s
          -- Parse: @{foo,bar}+
          [AST.T_Literal _ "@", AST.T_BraceExpansion _ items, AST.T_Literal _ s] ->
            buildQuantifiedChoice items $ Just s
          -- Parse: @(...)
          [AST.T_Extglob _ "@" [inner]] ->
            buildGroup inner Nothing
          -- Parse: @(...)? / @(...)*
          [AST.T_Extglob _ "@" [inner], AST.T_Glob _ s] ->
            buildGroup inner $ Just s
          -- Parse: @(...)+
          [AST.T_Extglob _ "@" [inner], AST.T_Literal _ s] ->
            buildGroup inner $ Just s
          -- Do some extra validation.
          (AST.T_Literal _ "@") : (AST.T_Literal _ "{") : _ ->
            Left ["Choice expressions @{...} must contain 2 or more elements."]
          (AST.T_Literal _ "@") : _ ->
            Left ["Unknown meta var @."]
          -- Anything else: take literally.
          _ -> Right (Token t')
        -- Anything else: also take literally.
        _ -> Right (Token t')
    buildGroup inner equant = do
      case literalText inner True of
        Just text -> do
          case simpleCommands <$> prRoot (parseShellScript text) of
            Just [AST.T_SimpleCommand _ _ list] -> do
              group <- mapM asMetaPart list
              maybeQuantWrap (Group group) equant
            _ -> Left ["Unable to parse group expression: @(" <> text <> ")."]
        _ -> Left ["No text for group expression @(...)."]
    buildQuantifiedChoice items equant = do
      choice <- Choice <$> mapM asMetaPart items
      maybeQuantWrap choice equant

parseMetaExpr :: Parsec String () Part
parseMetaExpr = do
  meta <- P.choice $ map P.try [P.string "arg", P.string "int", P.string "path", P.string "@", P.string "*"]
  let meta' = case meta of
        "arg" -> Just Arg
        "int" -> Just Int
        "path" -> Just Path
        "@" -> Just At
        "*" -> Just $ Quant Arg (Count 0 Nothing) -- @* shortcut.
        _ -> Nothing
  result <- case meta' of
    Just m@(Quant _ _) -> pure m
    Just m -> do
      atEnd <- P.optionMaybe P.anyChar
      case atEnd of
        Nothing -> pure m
        Just quant ->
          case parseQuantifier [quant] m of
            Just wrapped -> pure wrapped
            _ -> fail $ "Unknown quantifier '" ++ [quant] ++ "'."
    Nothing -> fail "Unknown meta var."
  P.eof
  pure result

maybeQuantWrap :: Part -> Maybe String -> Either [Text] Part
maybeQuantWrap p equant =
  case equant of
    Just quant -> do
      let ewrapped = parseQuantifier quant p
      case ewrapped of
        Just wrapped -> Right wrapped
        _ -> Left [T.pack $ "Unknown quantifier: " ++ quant ++ "."]
    Nothing -> Right p

parseQuantifier :: String -> Part -> Maybe Part
parseQuantifier s part = case s of
  "*" -> Just $ Quant part (Count 0 Nothing)
  "+" -> Just $ Quant part (Count 1 Nothing)
  "?" -> Just $ Quant part (Count 0 (Just 1))
  _ -> Nothing

-- | Entry point for Localmost logic. Given a Runtime (with configuration)
-- and a script, decides on a policy. Note the function does not depend on
-- IO as all IO is done from other functions.
computePolicy :: Runtime -> Script -> Policy
computePolicy rt input =
  let rules = rRules rt
      input' = applyRules rules input
   in fromMaybe Ask (scriptPolicy input')

scriptPolicy :: Script -> Maybe Policy
scriptPolicy script =
  let cmds = sCommands script
      policies = mapMaybe cmdPolicy cmds
   in if length cmds == length policies
        then Just $ maximum policies
        else Nothing

applyRules :: [Rule] -> Script -> Script
applyRules rules input = foldl (flip applyRule) input rules

applyRule :: Rule -> Script -> Script
applyRule rule input =
  let inputCommands = sCommands input
      policy = rPolicy rule
      matchAndApply c =
        if commandsMatch rule c
          then applyPolicy c policy
          else c
      transformed = map matchAndApply inputCommands
   in input {sCommands = transformed}

applyPolicy :: Command -> Policy -> Command
applyPolicy cmd policy = cmd {cmdPolicy = Just $ maybe policy (max policy) (cmdPolicy cmd)}

commandsMatch :: Rule -> Command -> Bool
commandsMatch rule@(Rule {rCommand = cmd}) input =
  allPartsMatch (cmdParts cmd) (cmdParts input)
    && pipesMatch rule input
    && redirectsMatch rule input
    && not (exceptsMatch rule input)

exceptsMatch :: Rule -> Command -> Bool
exceptsMatch Rule {rExcepts = excepts} input =
  let cmdHasNonConsts = not $ all isLiteral [tok | (Token tok) <- cmdParts input]
   in not (null excepts) && (cmdHasNonConsts || any (`matchOne` input) excepts)
  where
    matchOne (Except parts) cmd = allPartsMatch parts (cmdParts cmd)
    isLiteral tok = isJust $ literalText tok False

pipesMatch :: Rule -> Command -> Bool
pipesMatch Rule {rPipeAccess = pa} input = case pa of
  All -> True
  None -> not (cmdPipeIn input) && not (cmdPipeOut input)
  Out -> (not . cmdPipeIn) input
  In -> (not . cmdPipeOut) input

redirectsMatch :: Rule -> Command -> Bool
redirectsMatch Rule {rRedirectAccess = ra} input = case ra of
  RAAll -> True
  RANone -> null (cmdRedirects input)
  Safe -> all isSafe (cmdRedirects input)
  where
    isSafe r@(StaticPath _ m) = r `elem` safePaths || m == Read
    isSafe (DynamicPath _ m) = m == Read
    safePaths =
      [ StaticPath "/dev/null" Write,
        StaticPath "/dev/null" Read,
        StaticPath "/dev/zero" Read,
        StaticPath "/dev/urandom" Read,
        StaticPath "/dev/random" Read
      ]

allPartsMatch :: [Part] -> [Part] -> Bool
allPartsMatch rparts = execute (compile rparts)

data Instr = Match Part | Fork Int | Jump Int | Accept deriving (Show, Eq)

compile :: [Part] -> [Instr]
compile parts = emit parts ++ [Accept]
  where
    emit [] = []
    emit (Choice alts : rest) = emitChoice alts ++ emit rest
    emit (Group gparts : rest) = emit gparts ++ emit rest
    emit (Quant inner c : rest) = emitQuant inner c ++ emit rest
    emit (p : rest) = Match p : emit rest

    emitChoice [] = []
    emitChoice [a] = emit [a]
    emitChoice (a : as) =
      let bodyA = emit [a]
          restC = emitChoice as
       in Fork (length bodyA + 2) : bodyA ++ [Jump (1 + length restC)] ++ restC

    emitQuant inner (Count mn mx) =
      let body = emit [inner]
          blen = length body
          mandatory = concat (replicate mn body)
       in case mx of
            Nothing -> mandatory ++ [Fork (blen + 2)] ++ body ++ [Jump (-(blen + 1))]
            Just mx' ->
              let remaining = mx' - mn
                  opt 0 = []
                  opt n =
                    let rest = opt (n - 1)
                     in Fork (blen + 1 + length rest) : body ++ rest
               in mandatory ++ opt remaining

execute :: [Instr] -> [Part] -> Bool
execute instrs = go (IntSet.singleton 0)
  where
    go states [] = any isAccepted $ IntSet.toList (advance states)
    go states (tok : rest) =
      let expanded = advance states
          next = IntSet.fromList $ concatMap (step tok) $ IntSet.toList expanded
       in go next rest

    instrAt pc = if pc < length instrs then Just (instrs !! pc) else Nothing
    isAccepted pc = instrAt pc == Just Accept

    -- Advance through Fork and Jump (epsilon transitions) until fixpoint
    advance states =
      let next = IntSet.unions $ map expand $ IntSet.toList states
       in if next == states then states else advance next
    expand pc = case instrAt pc of
      Just (Fork off) -> IntSet.fromList [pc + 1, pc + off]
      Just (Jump off) -> IntSet.singleton (pc + off)
      _ -> IntSet.singleton pc

    -- Try consuming one token at a given state
    step tok pc = case instrAt pc of
      Just (Match p) -> [pc + 1 | partMatches p tok]
      _ -> []

partMatches :: Part -> Part -> Bool
partMatches rule input = case input of
  Token t ->
    case literalText t False of
      Just inputText -> case rule of
        Arg -> True
        Int -> isInt inputText
        At -> inputText == "@"
        Path -> isValid $ unpack inputText
        Token rt ->
          let rText = literalText rt True
           in rText == Just inputText
        _ -> False
      Nothing -> False
  _ -> False

check :: Proto -> IO ()
check proto = do
  econfig <- loadConfig
  case econfig of
    Right config -> case parseConfig config of
      Right runtime -> do
        etext <- pReadInput proto
        case etext of
          Left errs -> pWriteErrors proto errs
          Right cmd -> case astAsScript (parseShellScript cmd) False of
            Right input -> pWritePolicy proto (computePolicy runtime input)
            Left errs -> pWriteErrors proto errs
      Left errs ->
        pWriteErrors
          proto
          [ pack $
              "Found "
                ++ show (length errs)
                ++ " rule error(s) in configuration file.\n"
                ++ "Run 'localmost validate' to see more details."
          ]
    Left err -> pWriteErrors proto [err]

validate :: IO ()
validate = do
  path <- configPath
  putStrLn $ "Validating configuration at " ++ path ++ "..."
  econfig <- loadConfig
  case econfig of
    Right config -> do
      when (isNothing $ cPath config) $ do
        putStrLn "No configuration file found."
        exitFailure
      case parseConfig config of
        Right _ -> putStrLn "Configuration is valid."
        Left errs -> do
          mapM_ (ePutStrLn . show) errs
          exitFailure
    Left err -> do
      ePutStrLn (unpack err)
      exitFailure

ast :: Bool -> IO ()
ast isRule = do
  input <- getContents
  let pr = parseShellScript (pack input)
  let mast = prRoot pr
  for_ mast print
  let escript = astAsScript pr isRule
  case escript of
    Right script -> print script
    Left e -> do
      ePutStrLn $ "Failed to parse script: " ++ show e ++ "."
      exitFailure

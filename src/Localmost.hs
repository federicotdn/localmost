module Localmost
  ( ast,
    check,
    validate,
    parseConfig,
    computePolicy,
    astAsScript,
    Runtime (..),
    Script (..),
    Command (..),
    Part (..),
    Count (..),
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
    parseShellScript,
    simpleCommands,
    tokenAsText,
  )
import ShellCheck.AST qualified as AST
import ShellCheck.ASTLib qualified as ASTL
import ShellCheck.Interface (ParseResult (..))
import System.Exit (exitFailure)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as R
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

data Rule = Rule
  { rCommand :: Command,
    rPolicy :: Policy,
    rPipeAccess :: PipeAccess,
    rRedirectAccess :: RedirectAccess
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

parseRule :: Policy -> ConfigRule -> Either ParseRuleError Rule
parseRule pol r =
  let pr = parseShellScript (rRule r)
      escript = astAsScript pr True
      err msgs = Left ParseRuleError {preErrors = msgs, preComments = comments pr, preRule = r}
   in case escript of
        Right Script {sCommands = [c]} ->
          if cmdAllRedirectsCount c == 0
            then
              let pa = fromMaybe All (rPipe r) -- Allow all pipes by default.
                  ra = fromMaybe Safe (rRedirect r) -- Allow safe redirects by default.
               in Right Rule {rCommand = c, rPolicy = pol, rPipeAccess = pa, rRedirectAccess = ra}
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
      case tokenAsText f False of
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
  let mtext = tokenAsText t True
      hasPrefix = maybe False ("@" `T.isPrefixOf`) mtext
      expr = maybe "" (T.drop 1) mtext
      parsed = R.readP_to_S parseMetaExpr (T.unpack expr)
      quoted = isQuoted t
   in if hasPrefix && not quoted
        then case parsed of
          (r : _) -> Right (fst r)
          _ -> Left ["Unknown meta var: " <> fromMaybe "" mtext <> "."]
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
          -- Anything else: take literally.
          _ -> Right (Token t')
        -- Anything else: also take literally.
        _ -> Right (Token t')
    buildGroup inner equant = do
      case tokenAsText inner True of
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

parseMetaExpr :: ReadP Part
parseMetaExpr = do
  meta <- R.choice $ map R.string ["arg", "int", "anything", "@"]
  let meta' = case meta of
        "arg" -> Just Arg
        "int" -> Just Int
        "anything" -> Just PAnything
        "@" -> Just At
        _ -> Nothing
  result <- case meta' of
    Just m -> do
      atEnd <- null <$> R.look
      if atEnd
        then pure m
        else do
          if m == PAnything
            then R.pfail -- Anything can't have quantifiers
            else do
              quant <- R.get
              let ewrapped = parseQuantifier [quant] m
              case ewrapped of
                Just wrapped -> pure wrapped
                _ -> R.pfail
    Nothing -> R.pfail
  R.eof
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
  allPartsMatch (cmdParts cmd) (cmdParts input) && pipesMatch rule input && redirectsMatch rule input

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
    -- @anything is basically @arg*, but also matches splitting tokens.
    emit (PAnything : rest) = [Fork 3, Match PAnything, Jump (-2)] ++ emit rest
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

isSplitting :: Part -> Bool
isSplitting (Token t@(AST.T_NormalWord _ _)) = ASTL.willSplit t
isSplitting _ = False

partMatches :: Part -> Part -> Bool
partMatches rule input = case (rule, input) of
  (PAnything, _) -> True
  (Arg, _) -> (not . isSplitting) input
  (Int, Token t) -> maybe False isInt (tokenAsText t False)
  (At, Token t) -> tokenAsText t False == Just "@"
  (Token rt, Token it) ->
    let rText = tokenAsText rt True
        eq = rt == it
     in (isJust rText && rText == tokenAsText it False) || eq
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

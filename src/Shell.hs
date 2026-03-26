module Shell
  ( parseShellScript,
    comments,
    isQuoted,
    literalText,
    simpleCommands,
    fullCommands,
  )
where

import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.Text (Text, pack, unpack)
import ShellCheck.AST qualified as AST
import ShellCheck.ASTLib qualified as ASTL
import ShellCheck.Interface
  ( Comment (..),
    ParseResult (..),
    ParseSpec (..),
    Position (..),
    PositionedComment (..),
    Severity (..),
    Shell (..),
    mockedSystemInterface,
    newParseResult,
    newParseSpec,
  )
import ShellCheck.Parser (parseScript)

comments :: ParseResult -> [Text]
comments pr = map formatComment (prComments pr)

formatComment :: PositionedComment -> Text
formatComment pc =
  pack $
    "<input>:"
      <> show (posLine pos)
      <> ":"
      <> show (posColumn pos)
      <> ": "
      <> severityStr (cSeverity c)
      <> ": "
      <> cMessage c
      <> " [SC"
      <> show (cCode c)
      <> "]"
  where
    pos = pcStartPos pc
    c = pcComment pc
    severityStr ErrorC = "error"
    severityStr WarningC = "warning"
    severityStr InfoC = "note"
    severityStr StyleC = "style"

parseShellScript :: Text -> ParseResult
parseShellScript "" = newParseResult -- Nothing as prRoot
parseShellScript script =
  let iface = mockedSystemInterface []
      spec = newParseSpec {psScript = unpack script, psShellTypeOverride = Just Bash}
   in runIdentity $ parseScript iface spec

literalText :: AST.Token -> Bool -> Maybe Text
literalText t globs =
  let fn = if globs then ASTL.getGlobOrLiteralString else ASTL.getLiteralString
   in pack <$> fn t

isQuoted :: AST.Token -> Bool
isQuoted (AST.OuterToken _ inner) = case inner of
  AST.Inner_T_SingleQuoted _ -> True
  AST.Inner_T_DoubleQuoted _ -> True
  AST.Inner_T_NormalWord list -> any isQuoted list
  _ -> False

simpleCommands :: AST.Token -> [AST.Token]
simpleCommands root = execWriter (AST.doAnalysis collect root)
  where
    collect :: AST.Token -> Writer [AST.Token] ()
    collect t@(AST.T_SimpleCommand {}) = tell [t]
    collect _ = pure ()

fullCommands :: AST.Token -> [(AST.Token, AST.Token)]
fullCommands root = execWriter (AST.doAnalysis collect root)
  where
    collect :: AST.Token -> Writer [(AST.Token, AST.Token)] ()
    collect p@(AST.T_Pipeline _ _ cmds) =
      for_ cmds $ \r -> case r of
        AST.T_Redirecting _ _ (AST.T_SimpleCommand {}) -> tell [(p, r)]
        _ -> pure ()
    collect _ = pure ()

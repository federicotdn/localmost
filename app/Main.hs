module Main where

import Config (initConfig)
import Localmost (ast, check, validate)
import Options.Applicative
  ( Parser,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    option,
    progDesc,
    short,
    str,
    switch,
    value,
    (<**>),
  )
import Protocol (claudeCode, simpleText)

newtype AstOpts = AstOpts {aoRule :: Bool}

newtype CheckOpts = CheckOpts {coMode :: String}

data Command = Check CheckOpts | Validate | Init | Ast AstOpts

newtype Options = Options {oCommand :: Command}

checkCommand :: Parser Command
checkCommand = Check . CheckOpts <$> option str (long "mode" <> short 'm' <> value "claude" <> help "Protocol mode: claude, text.")

validateCommand :: Parser Command
validateCommand = pure Validate

initCommand :: Parser Command
initCommand = pure Init

astCommand :: Parser Command
astCommand = Ast . AstOpts <$> switch (long "rule" <> help "Parse input as a rule (resolve meta vars).")

parser :: Parser Options
parser =
  Options
    <$> hsubparser
      ( command "check" (info checkCommand (progDesc "Check if bash command should be run."))
          <> command "validate" (info validateCommand (progDesc "Validate the config.json file."))
          <> command "init" (info initCommand (progDesc "Create a default config.json file."))
          <> command "ast" (info astCommand (progDesc "Parse a script and print its AST (debugging)."))
      )

main :: IO ()
main = do
  let fullParser =
        info
          (parser <**> helper)
          (fullDesc <> header "localmost - A Claude Code PreToolUse tool based on ShellCheck.")

  opts <- execParser fullParser

  case oCommand opts of
    Check checkOpts ->
      let proto = case coMode checkOpts of
            "text" -> simpleText
            _ -> claudeCode
       in check proto
    Validate -> validate
    Init -> initConfig
    Ast astOpts -> ast (aoRule astOpts)

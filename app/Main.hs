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
    progDesc,
    switch,
    (<**>),
  )
import Protocol (claudeCode)

newtype AstOpts = AstOpts {aoRule :: Bool}

data Command = Check | Validate | Init | Ast AstOpts

newtype Options = Options {oCommand :: Command}

checkCommand :: Parser Command
checkCommand = pure Check

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
          <> command "ast" (info astCommand (progDesc "Parse a script and print its AST."))
      )

main :: IO ()
main = do
  let fullParser =
        info
          (parser <**> helper)
          (fullDesc <> header "localmost - A Claude Code PreToolUse tool based on ShellCheck.")

  opts <- execParser fullParser

  case oCommand opts of
    Check -> check claudeCode
    Validate -> validate
    Init -> initConfig
    Ast astOpts -> ast (aoRule astOpts)

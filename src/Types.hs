module Types
  ( Policy (..),
    Command (..),
    PipeAccess (..),
    RedirectAccess (..),
    RedirectMode (..),
    Redirect (..),
    Script (..),
    Count (..),
    Part (..),
    Errors,
    showToken,
  )
where

import Control.Monad (void)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    genericParseJSON,
    genericToJSON,
    parseJSON,
    toJSON,
  )
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import ShellCheck.AST qualified as AST
import Utils (jsonOptions)

type Errors = [Text]

data Policy
  = Allow
  | Ask
  | Deny
  deriving (Eq, Ord, Generic, Show)

newtype Script = Script {sCommands :: [Command]} deriving (Show, Eq)

data Command = Command
  { cmdParts :: [Part],
    cmdPolicy :: Maybe Policy, -- Only used for input commands, not rules.
    cmdPipeIn :: Bool,
    cmdPipeOut :: Bool,
    -- Counts all redirections.
    cmdAllRedirectsCount :: Int,
    -- Redirections to files only.
    cmdRedirects :: [Redirect]
  }
  deriving (Show, Eq)

data RedirectMode = Read | Write deriving (Show, Eq)

data Redirect
  = StaticPath Text RedirectMode
  | DynamicPath AST.Token RedirectMode
  deriving (Show, Eq)

data PipeAccess = All | None | Out | In deriving (Show, Eq)

data RedirectAccess
  = RAAll
  | RANone
  | Safe
  deriving (Show, Eq)

data Count = Count Int (Maybe Int) deriving (Eq)

data Part
  = Token AST.Token
  | Literal Text
  | Choice [Part]
  | Group [Part]
  | Arg
  | Int
  | At
  | Path
  | Quant Part Count
  deriving (Eq)

instance Show Count where
  show (Count mn Nothing) = "Count " ++ show mn ++ " *"
  show (Count mn (Just mx)) = "Count " ++ show mn ++ " " ++ show mx

instance Show Part where
  show (Token t) = "Token (" ++ show (AST.getId t) ++ ") (" ++ showToken t ++ ")"
  show (Literal t) = unpack $ "Literal " <> t
  show (Choice list) = "Choice " ++ show list
  show (Group list) = "Group " ++ show list
  show Arg = "Arg"
  show Int = "Int"
  show At = "At"
  show Path = "Path"
  show (Quant p c) = "Quant " ++ show p ++ " " ++ show c

instance FromJSON Policy where parseJSON = genericParseJSON jsonOptions

instance ToJSON Policy where toJSON = genericToJSON jsonOptions

instance FromJSON PipeAccess where
  parseJSON (Bool True) = pure All
  parseJSON (Bool False) = pure None
  parseJSON (String "out") = pure Out
  parseJSON (String "in") = pure In
  parseJSON _ = fail "expected true, false, \"in\", or \"out\""

instance ToJSON PipeAccess where
  toJSON All = Bool True
  toJSON None = Bool False
  toJSON Out = String "out"
  toJSON In = String "in"

instance FromJSON RedirectAccess where
  parseJSON (Bool True) = pure RAAll
  parseJSON (Bool False) = pure RANone
  parseJSON (String "safe") = pure Safe
  parseJSON _ = fail "expected true, false, or \"safe\""

instance ToJSON RedirectAccess where
  toJSON RAAll = Bool True
  toJSON RANone = Bool False
  toJSON Safe = String "safe"

showToken :: AST.Token -> String
showToken (AST.OuterToken _ inner) =
  case words (show (void inner)) of
    (name : _) -> name
    [] -> "<Unknown>"

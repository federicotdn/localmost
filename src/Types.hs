module Types
  ( Policy (..),
    PipeAccess (..),
    RedirectAccess (..),
    Errors,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    genericParseJSON,
    genericToJSON,
    parseJSON,
    toJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Utils (jsonOptions)

type Errors = [Text]

data Policy
  = Allow
  | Ask
  | Deny
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Policy where parseJSON = genericParseJSON jsonOptions

instance ToJSON Policy where toJSON = genericToJSON jsonOptions

data PipeAccess = All | None | Out | In deriving (Show, Eq)

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

data RedirectAccess
  = RAAll
  | RANone
  | Safe
  deriving (Show, Eq)

instance FromJSON RedirectAccess where
  parseJSON (Bool True) = pure RAAll
  parseJSON (Bool False) = pure RANone
  parseJSON (String "safe") = pure Safe
  parseJSON _ = fail "expected true, false, or \"safe\""

instance ToJSON RedirectAccess where
  toJSON RAAll = Bool True
  toJSON RANone = Bool False
  toJSON Safe = String "safe"

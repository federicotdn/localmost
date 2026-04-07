module Protocol
  ( Proto (..),
    claudeCode,
    simpleText,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    eitherDecode,
    encode,
    genericParseJSON,
    genericToJSON,
    parseJSON,
    toJSON,
  )
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Text (Text, intercalate, pack)
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Types (Errors, Policy (..))
import Utils (ePutStrLn, jsonOptions, jsonOptionsLax)

-- | Proto separates the actual IO operations of reading and writing scripts, results and
-- errors from the real validation logic. In theory, implementing the functions below could
-- allow Localmost to work with other AI agents, or with any other tool.
data Proto = Proto
  { pReadInput :: IO (Either Errors Text),
    pWritePolicy :: Policy -> IO (),
    pWriteErrors :: Errors -> IO ()
  }

---- Claude Code

data HookEvent = HookEvent
  { hCwd :: Maybe Text, -- TODO: Unused
    hTool_input :: HookEventInput
  }
  deriving (Generic, Show)

instance FromJSON HookEvent where parseJSON = genericParseJSON jsonOptionsLax

newtype HookEventInput = HookEventInput {iCommand :: Text} deriving (Generic, Show)

instance FromJSON HookEventInput where parseJSON = genericParseJSON jsonOptionsLax

data HookSpecificOutput = HookSpecificOutput
  { oHookEventName :: Text,
    oPermissionDecision :: Policy,
    oPermissionDecisionReason :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON HookSpecificOutput where toJSON = genericToJSON jsonOptions

newtype HookResponse = HookResponse {rHookSpecificOutput :: HookSpecificOutput} deriving (Generic, Show)

instance ToJSON HookResponse where toJSON = genericToJSON jsonOptions

policyResponse :: Policy -> HookResponse
policyResponse p = HookResponse $ HookSpecificOutput "PreToolUse" p Nothing

errResponse :: Errors -> HookResponse
errResponse errs =
  HookResponse $ HookSpecificOutput "PreToolUse" Ask (Just $ "[localmost] Error: " <> intercalate ", " errs)

writeResponse :: HookResponse -> IO ()
writeResponse = BL.putStr . encode

readInput :: IO (Either Errors Text)
readInput = do
  input <- BL.getContents
  case eitherDecode input of
    Left err -> pure $ Left [pack err]
    Right event -> pure $ Right $ iCommand (hTool_input event)

claudeCode :: Proto
claudeCode =
  Proto
    { pReadInput = readInput,
      pWritePolicy = writeResponse . policyResponse,
      pWriteErrors = writeResponse . errResponse
    }

---- Simple

simpleText :: Proto
simpleText =
  Proto
    { pReadInput = Right <$> TIO.getContents,
      pWritePolicy = putStrLn . map toLower . show,
      pWriteErrors = \errs -> ePutStrLn $ "[localmost] Error: " ++ show (intercalate ", " errs)
    }

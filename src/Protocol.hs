module Protocol
  ( Proto (..),
    Input (..),
    PolicyOutput (..),
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

data Input = Input {iText :: Text, iInteractive :: Bool}

data PolicyOutput = PolicyOutput {pPolicy :: Policy, pReason :: Maybe Text}

-- | Proto separates the actual IO operations of reading and writing scripts, results and
-- errors from the real validation logic. In theory, implementing the functions below could
-- allow Localmost to work with other AI agents, or with any other tool.
data Proto = Proto
  { pReadInput :: IO (Either Errors Input),
    pWritePolicy :: PolicyOutput -> IO (),
    pWriteErrors :: Errors -> IO ()
  }

---- Claude Code

data HookEvent = HookEvent
  { hCwd :: Maybe Text, -- TODO: Unused
    hTool_input :: HookEventInput,
    hPermission_mode :: Maybe Text
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

policyResponse :: PolicyOutput -> HookResponse
policyResponse (PolicyOutput p reason) = HookResponse $ HookSpecificOutput "PreToolUse" p reason

errResponse :: Errors -> HookResponse
errResponse errs =
  HookResponse $ HookSpecificOutput "PreToolUse" Ask (Just $ "[localmost] Error: " <> intercalate ", " errs)

writeResponse :: HookResponse -> IO ()
writeResponse = BL.putStr . encode

readInput :: IO (Either Errors Input)
readInput = do
  input <- BL.getContents
  case eitherDecode input of
    Left err -> pure $ Left [pack err]
    Right event ->
      pure $
        Right $
          Input
            { iText = iCommand (hTool_input event),
              iInteractive = hPermission_mode event /= Just "acceptEdits"
            }

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
    { pReadInput = Right . (`Input` True) <$> TIO.getContents,
      pWritePolicy = putStrLn . map toLower . show . pPolicy,
      pWriteErrors = \errs -> ePutStrLn $ "[localmost] Error: " ++ show (intercalate ", " errs)
    }

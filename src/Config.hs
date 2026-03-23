module Config
  ( ConfigRule (..),
    Config (..),
    configPath,
    loadConfig,
    defaultConfig,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import Types (PipeAccess (..), RedirectAccess (..))
import Utils (jsonOptions, tryIO)

data ConfigRule = ConfigRule
  { rRule :: Text,
    rPipe :: Maybe PipeAccess,
    rRedirect :: Maybe RedirectAccess
  }
  deriving (Generic, Show, Eq)

instance FromJSON ConfigRule where parseJSON = genericParseJSON jsonOptions

instance ToJSON ConfigRule where toJSON = genericToJSON jsonOptions

data Config = Config
  { cAllow :: Maybe [ConfigRule],
    cDeny :: Maybe [ConfigRule],
    cPath :: Maybe FilePath
  }
  deriving (Generic, Show)

instance FromJSON Config where parseJSON = genericParseJSON jsonOptions

instance ToJSON Config where toJSON = genericToJSON jsonOptions

configPath :: IO FilePath
configPath = do
  dir <- getXdgDirectory XdgConfig "localmost"
  pure $ dir </> "config.json"

defaultConfig :: Config
defaultConfig = Config Nothing Nothing Nothing

loadConfig :: IO (Either Text Config)
loadConfig = do
  path <- configPath
  exists <- doesFileExist path
  if not exists
    then pure $ Right defaultConfig
    else do
      bytes <- tryIO (BL.readFile path)
      case bytes of
        Nothing -> pure $ Left $ "Could not load configuration file (" <> pack path <> ")"
        Just b -> case eitherDecode b of
          Left err -> pure $ Left ("Invalid configuration file: " <> pack err)
          Right cfg -> pure $ Right cfg {cPath = Just path}

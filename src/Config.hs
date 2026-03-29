module Config
  ( ConfigRule (..),
    Config (..),
    configPath,
    loadConfig,
    defaultConfig,
    initConfig,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))
import Types (PipeAccess (..), RedirectAccess (..))
import Utils (jsonOptions, tryIO)

data ConfigRule = ConfigRule
  { rRule :: Text,
    rPipe :: Maybe PipeAccess,
    rRedirect :: Maybe RedirectAccess,
    rUnless :: Maybe [Text]
  }
  deriving (Generic, Show, Eq)

instance FromJSON ConfigRule where parseJSON = genericParseJSON jsonOptions

instance ToJSON ConfigRule where toJSON = genericToJSON jsonOptions

data Config = Config
  { cAllow :: Maybe [ConfigRule],
    cDeny :: Maybe [ConfigRule],
    cAllowSafeXargs :: Maybe Bool,
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
defaultConfig = Config Nothing Nothing Nothing Nothing

initConfig :: IO ()
initConfig = do
  path <- configPath
  exists <- doesFileExist path
  if exists
    then putStrLn $ "Configuration file already exists at " ++ path ++ "."
    else do
      createDirectoryIfMissing True (takeDirectory path)
      let rule = ConfigRule {rRule = "echo @arg*", rPipe = Nothing, rRedirect = Nothing, rUnless = Nothing}
      let emptyConfig = Config {cAllow = Just [rule], cDeny = Nothing, cPath = Nothing, cAllowSafeXargs = Nothing}
      BL.writeFile path (encode emptyConfig)
      putStrLn $ "Created configuration file at " ++ path ++ "."

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

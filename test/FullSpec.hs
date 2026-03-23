module FullSpec where

import Config (Config (..))
import Data.Aeson (FromJSON, eitherDecode, genericParseJSON, parseJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import Localmost (Runtime, computePolicy, parseConfig)
import Test.Hspec
import TestUtils (right, sh)
import Types (Policy (..))
import Utils (jsonOptions)

data TestSuite = TestSuite
  { sName :: Text,
    sConfig :: Config,
    sCases :: [TestCase]
  }
  deriving (Generic)

data TestCase = TestCase
  { cInput :: Text,
    cExpect :: Policy,
    cComment :: Maybe Text
  }
  deriving (Generic)

instance FromJSON TestSuite where parseJSON = genericParseJSON jsonOptions

instance FromJSON TestCase where parseJSON = genericParseJSON jsonOptions

spec :: Spec
spec = do
  suites <- runIO $ do
    bytes <- BL.readFile "test/test_cases.json"
    case eitherDecode bytes of
      Left err -> fail $ "Failed to parse test_cases.json: " ++ err
      Right s -> pure (s :: [TestSuite])
  describe "test_cases.json" $
    mapM_ runSuite suites

runSuite :: TestSuite -> Spec
runSuite suite = do
  let rt = right $ parseConfig (sConfig suite)
  describe (show (sName suite)) $
    mapM_ (runCase rt) (sCases suite)

runCase :: Runtime -> TestCase -> Spec
runCase rt tc =
  it (show (cInput tc) ++ " -> " ++ show (cExpect tc)) $
    computePolicy rt (sh (cInput tc)) `shouldBe` cExpect tc

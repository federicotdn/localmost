module ConfigSpec where

import Config
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultConfig" $ do
    it "returns a safe default config" $ do
      let config = defaultConfig
      cAllow config `shouldBe` Nothing
      cDeny config `shouldBe` Nothing

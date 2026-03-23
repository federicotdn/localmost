module ShellSpec where

import Shell
import ShellCheck.Interface (ParseResult (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseShellScript" $ do
    it "rejects empty text" $ do
      let pr = parseShellScript ""
      prRoot pr `shouldBe` Nothing

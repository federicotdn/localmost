module Main (main) where

import ConfigSpec qualified
import FullSpec qualified
import LocalmostSpec qualified
import ShellSpec qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Localmost" LocalmostSpec.spec
  describe "Shell" ShellSpec.spec
  describe "Config" ConfigSpec.spec
  describe "Full" FullSpec.spec

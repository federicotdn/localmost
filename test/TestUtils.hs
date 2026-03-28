module TestUtils
  ( right,
    sh,
    shr,
  )
where

import Data.Either (fromRight)
import Data.Text (Text)
import Localmost
import Shell

right :: (Show a, Show b) => Either a b -> b
right e = fromRight (error $ "expected Right, got " ++ show e) e

sh :: Text -> Script
sh s = right $ astAsScript (parseShellScript s) False

-- TODO: Use this helper to write some unit tests
shr :: Text -> Script
shr s = right $ astAsScript (parseShellScript s) True

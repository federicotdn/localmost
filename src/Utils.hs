module Utils
  ( tryIO,
    jsonOptions,
    jsonOptionsLax,
    ePutStrLn,
    isInt,
  )
where

import Data.Aeson
  ( Options,
    constructorTagModifier,
    defaultOptions,
    fieldLabelModifier,
    omitNothingFields,
    rejectUnknownFields,
  )
import Data.Char (toLower)
import Data.Text (Text, unpack)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)

tryIO :: IO a -> IO (Maybe a)
tryIO op = either (const Nothing) Just <$> tryIOError op

dropPrefixAndLower :: String -> String
dropPrefixAndLower (_ : c : rest) = toLower c : rest
dropPrefixAndLower [_] = ""
dropPrefixAndLower [] = ""

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = dropPrefixAndLower,
      constructorTagModifier = map toLower,
      omitNothingFields = True,
      rejectUnknownFields = True
    }

jsonOptionsLax :: Options
jsonOptionsLax = jsonOptions {rejectUnknownFields = False}

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

isInt :: Text -> Bool
isInt s = case reads (unpack s) :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

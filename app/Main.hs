module Main where

import           Data.Char                      ( isSpace )
import           Text.Megaparsec                ( parseMaybe )
import           Local.Data.JSON.Parser

-- Inefficient, but simple.
trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  json' <- parseMaybe json . trimEnd <$> getContents
  print json'

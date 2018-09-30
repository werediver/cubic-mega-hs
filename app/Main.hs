module Main where

import           Text.Megaparsec                ( parseTest )
import           Local.Data.JSON.Parser

main :: IO ()
main = getContents >>= parseTest json

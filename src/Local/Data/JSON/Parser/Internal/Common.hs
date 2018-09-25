module Local.Data.JSON.Parser.Internal.Common (
    module Local.Data.JSON.Parser.Internal.Common,
    module Text.Megaparsec,
    module Text.Megaparsec.Char
  ) where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser a = Parsec Void String a

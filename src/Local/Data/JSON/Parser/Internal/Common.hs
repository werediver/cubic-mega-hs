module Local.Data.JSON.Parser.Internal.Common (
    module Local.Data.JSON.Parser.Internal.Common,
    module Text.Megaparsec,
    module Text.Megaparsec.Char
  ) where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Applicative

type Parser a = Parsec Void String a

-- Attempt map, a failable transformation
attmap :: (Alternative m, Monad m) => (a -> Maybe b) -> m a -> m b
attmap f ma =
  ma >>= \a -> case f a of
    Just b  -> pure b
    Nothing -> empty

(<&?>) :: (Alternative m, Monad m) => m a -> (a -> Maybe b) -> m b
(<&?>) = flip attmap

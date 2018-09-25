module Local.Data.JSON.Parser.Internal.Number (
    numberLiteral
  ) where

import           Local.Data.JSON.Parser.Internal.Common
import           Data.Maybe                     ( fromMaybe )
import           Data.Char                      ( isDigit )
import           Data.Ix                        ( inRange )

numberLiteral :: (Fractional a, Read a) => Parser a
numberLiteral =
  read <$> optionalMinusPart <?++> naturalPart <++?> optionalFractionPart <++?> optionalExponentPart
  where
    optionalMinusPart :: Parser (Maybe String)
    optionalMinusPart = optional $ string "-"
    naturalPart :: Parser String
    naturalPart = zeroCase <|> nonZeroCase
      where
        zeroCase = string "0"
        nonZeroCase = (:) <$> nonZeroDigit <*> digits
          where
            nonZeroDigit = satisfy (inRange ('1', '9')) <?> "non zero digit"
    optionalFractionPart :: Parser (Maybe String)
    optionalFractionPart = optional $ (:) <$> char '.' <*> digits
    optionalExponentPart :: Parser (Maybe String)
    optionalExponentPart = optional $ ((pure <$> oneOf "Ee") <++?> optionalSignPart) <++> digits
      where
        optionalSignPart :: Parser (Maybe String)
        optionalSignPart = optional $ pure <$> oneOf "+-"

    digits :: Parser String
    digits = takeWhileP (Just "digit") isDigit

    (<++>) :: Parser String -> Parser String -> Parser String
    (<++>) p q = (++) <$> p <*> q
    (<?++>) :: Parser (Maybe String) -> Parser String -> Parser String
    (<?++>) p q = (++) <$> (fromMaybe "" <$> p) <*> q
    (<++?>) :: Parser String -> Parser (Maybe String) -> Parser String
    (<++?>) p q = (++) <$> p <*> (fromMaybe "" <$> q)

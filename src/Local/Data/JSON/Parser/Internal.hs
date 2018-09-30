module Local.Data.JSON.Parser.Internal where

import           Numeric                        ( readHex )
import           Data.Char                      ( chr, isHexDigit )
import           Local.Data.JSON
import           Local.Data.JSON.Parser.Internal.Common
import           Local.Data.JSON.Parser.Internal.Number

import qualified Data.Map                      as Map

json :: Parser JSON
json = ws *> jsonValue

ws :: Parser String
ws =
  takeWhileP (Just "whitespace") $ \c -> c `elem` " \t\n\r"    

jsonValue :: Parser JSON
jsonValue =
      jsonObject
  <|> jsonArray
  <|> jsonNumber
  <|> jsonString
  <|> jsonBool
  <|> jsonNull

jsonObject :: Parser JSON
jsonObject =
  start *> (JSONObject . Map.fromList <$> members) <* end
  where
    start = char '{' <* ws
    end   = char '}' <* ws
    member =  let name = stringLiteral <* ws
                  sep  = char ':' <* ws
              in (,) <$> name <* sep <*> jsonValue
    members = member `sepBy` (char ',' <* ws)

jsonArray :: Parser JSON
jsonArray =
  start *> (JSONArray <$> items) <* end
  where
    start = char '[' <* ws
    end   = char ']' <* ws
    items = jsonValue `sepBy` (char ',' <* ws)

jsonNumber :: Parser JSON
jsonNumber =
  JSONNumber <$> numberLiteral <* ws

jsonString :: Parser JSON
jsonString =
  JSONString <$> stringLiteral <* ws

stringLiteral :: Parser String
stringLiteral =
  quote *> body <* quote
  where
    quote = char '"'
    body :: Parser String
    body = concat <$> many (unescapedChunk <|> escapeSequence)
      where
        unescapedChunk =
          takeWhile1P (Just "unescaped string literal character") isStringLiteralChar
          where
            isStringLiteralChar c = c /= '"' && c /= '\\' && c > '\x1F'
        escapeSequence :: Parser String
        escapeSequence =
          (:[]) <$> escapeSequence'
          where
            escapeSequence' :: Parser Char
            escapeSequence' =
              label "escape sequence" $
                char '\\' *> (singleCharEsc <|> unicodeEsc)
              where
                singleCharEsc :: Parser Char
                singleCharEsc = label "single-character escape sequence" $
                      oneOf "\"\\/"
                  <|> oneOf "bfnrt" <&?> \case
                    'b' -> Just '\b'
                    'f' -> Just '\f'
                    'n' -> Just '\n'
                    'r' -> Just '\r'
                    't' -> Just '\t'
                    _   -> Nothing
                unicodeEsc :: Parser Char
                unicodeEsc =
                  label "Unicode code point escape sequence" $
                    char 'u' *> (chr <$> hexNumber)
                  where
                    hexNumber :: Parser Int
                    hexNumber =
                      hexDigits <&?> readMaybeHex
                      where
                        hexDigits = takeWhileP (Just "hexadecimal digit") isHexDigit
                        readMaybeHex :: (Eq a, Num a) => String -> Maybe a
                        readMaybeHex = unpack . readHex
                          where
                            unpack ((x, _):_) = Just x
                            unpack []         = Nothing

jsonBool :: Parser JSON
jsonBool =
  JSONBool <$> boolLiteral <* ws

boolLiteral :: Parser Bool
boolLiteral = const True <$> string "true" <|> const False <$> string "false"

jsonNull :: Parser JSON
jsonNull = const JSONNull <$> string "null" <* ws

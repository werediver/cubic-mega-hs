module Local.Data.JSON.Parser.Internal where

import           Local.Data.JSON

import           Data.Void
import           Data.Char                      ( isDigit )
import           Data.Ix                        ( inRange )
import qualified Data.Map                      as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser a = Parsec Void String a

json :: Parser JSON
json = ws *> jsonValue

ws :: Parser String
ws =
  takeWhileP (Just "whitespace") $ \c -> c `elem` " \t\n\r"    

jsonValue :: Parser JSON
jsonValue =
      jsonObject
  <|> jsonArray
  <|> JSONNumber <$> numberLiteral <* ws
  <|> JSONString <$> stringLiteral <* ws
  <|> JSONBool   <$> boolLiteral   <* ws
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

numberLiteral :: Parser Double
numberLiteral = parse'
 where
  sign               = maybe 1 (const (-1)) <$> optional (char '-')
  signlessIntLiteral = 
    let zero    = string "0"
        nonZero =
          let nonZeroDigit = satisfy (inRange ('1', '9')) <?> "non-zero digit"
              digits       = takeWhileP (Just "digit") isDigit
          in (:) <$> nonZeroDigit <*> digits
    in zero <|> nonZero
  parse' =
    let combine sign' signlessIntLiteral' = sign' * (read signlessIntLiteral' :: Double)
    in combine <$> sign <*> signlessIntLiteral

stringLiteral :: Parser String
stringLiteral = quote *> body <* quote
 where
  quote = char '"'
  body  = takeWhileP (Just "string literal character") isStringLiteralChar
  isStringLiteralChar c = c /= '"' && c /= '\\' && c > '\x1F'

boolLiteral :: Parser Bool
boolLiteral = const True <$> string "true" <|> const False <$> string "false"

jsonNull :: Parser JSON
jsonNull = const JSONNull <$> string "null" <* ws

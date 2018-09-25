module Local.Data.JSON.Parser.Internal where

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
stringLiteral = quote *> body <* quote
  where
    quote = char '"'
    body  = takeWhileP (Just "string literal character") isStringLiteralChar
    isStringLiteralChar c = c /= '"' && c /= '\\' && c > '\x1F'

jsonBool :: Parser JSON
jsonBool =
  JSONBool <$> boolLiteral <* ws

boolLiteral :: Parser Bool
boolLiteral = const True <$> string "true" <|> const False <$> string "false"

jsonNull :: Parser JSON
jsonNull = const JSONNull <$> string "null" <* ws

module JSONSpec where

import qualified Data.Map                      as Map
import           Test.Hspec
import           Text.Megaparsec
import           Local.Data.JSON
import           Local.Data.JSON.Parser.Internal

spec :: Spec
spec = do

  describe "null parser" $
    it "parses null" $
      parseMaybe jsonNull "null" `shouldBe` Just JSONNull

  describe "bool parser" $ do
    it "parses false" $
      parseMaybe boolLiteral "false" `shouldBe` Just False
    it "parses true" $
      parseMaybe boolLiteral "true" `shouldBe` Just True

  describe "string parser" $ do
    it "parses empty string" $
      parseMaybe stringLiteral "\"\"" `shouldBe` Just ""
    it "parses non-empty string" $
      parseMaybe stringLiteral "\"qwerty\"" `shouldBe` Just "qwerty"

  describe "number parser" $ do
    it "parses zero" $
      parseMaybe jsonNumber "0" `shouldBe` Just (JSONNumber 0)
    it "parses negative zero" $
      parseMaybe jsonNumber "-0" `shouldBe` Just (JSONNumber 0)
    it "parses single digit number" $
      parseMaybe jsonNumber "1" `shouldBe` Just (JSONNumber 1)
    it "parses multiple digits number" $
      parseMaybe jsonNumber "123" `shouldBe` Just (JSONNumber 123)
    it "parses negative single digit number" $
      parseMaybe jsonNumber "-1" `shouldBe` Just (JSONNumber (-1))
    it "parses negative multiple digits number" $
      parseMaybe jsonNumber "-123" `shouldBe` Just (JSONNumber (-123))
    it "parses fractional part" $
      parseMaybe jsonNumber "1.012" `shouldBe` Just (JSONNumber 1.012)
    it "parses exponent without fractional part" $ do
      parseMaybe jsonNumber "1e2" `shouldBe` Just (JSONNumber 100)
      parseMaybe jsonNumber "1E2" `shouldBe` Just (JSONNumber 100)
      parseMaybe jsonNumber "1e+2" `shouldBe` Just (JSONNumber 100)
      parseMaybe jsonNumber "1e-2" `shouldBe` Just (JSONNumber 0.01)
    it "parses exponent after fractional part" $ do
      parseMaybe jsonNumber "1.012e3" `shouldBe` Just (JSONNumber 1012)
      parseMaybe jsonNumber "1.012E3" `shouldBe` Just (JSONNumber 1012)
      parseMaybe jsonNumber "1.012e+3" `shouldBe` Just (JSONNumber 1012)
      parseMaybe jsonNumber "1.012e-3" `shouldBe` Just (JSONNumber 0.001012)
  
  describe "array parser" $ do
    it "parses empty array without whitespace" $
      parseMaybe jsonArray "[]" `shouldBe` Just (JSONArray [])
    it "parses empty array with whitespace" $
      parseMaybe jsonArray "[ ]" `shouldBe` Just (JSONArray [])
    it "parses heterogenous array without whitespace" $
      parseMaybe jsonArray "[null,1]" `shouldBe` Just (JSONArray [JSONNull, JSONNumber 1])
    it "parses heterogenous array with whitespace" $
      parseMaybe jsonArray "[ null , 1 ]" `shouldBe` Just (JSONArray [JSONNull, JSONNumber 1])

  describe "object parser" $ do
    it "parses empty object without whitespace" $
      parseMaybe jsonObject "{}" `shouldBe` Just (JSONObject (Map.fromList []))
    it "parses empty object with whitespace" $
      parseMaybe jsonObject "{ }" `shouldBe` Just (JSONObject (Map.fromList []))
    it "parses non-empty object without whitespace" $
      parseMaybe jsonObject "{\"a\":1,\"b\":2}" `shouldBe` Just (JSONObject (Map.fromList [("a", JSONNumber 1), ("b", JSONNumber 2)]))
    it "parses non-empty object with whitespace" $
      parseMaybe jsonObject "{ \"a\" : 1 , \"b\" : 2 }" `shouldBe` Just (JSONObject (Map.fromList [("a", JSONNumber 1), ("b", JSONNumber 2)]))

  describe "JSON parser" $ do
    it "parses null" $
      parseMaybe json "null" `shouldBe` Just JSONNull
    it "parses bool" $
      parseMaybe json "false" `shouldBe` Just (JSONBool False)
    it "parses string" $
      parseMaybe json "\"\"" `shouldBe` Just (JSONString "")
    it "parses number" $
      parseMaybe json "0" `shouldBe` Just (JSONNumber 0)
    it "parses array" $
      parseMaybe json "[1]" `shouldBe` Just (JSONArray [JSONNumber 1])
    it "parses object" $
      parseMaybe json "{\"key\":null}" `shouldBe` Just (JSONObject (Map.fromList [("key", JSONNull)]))

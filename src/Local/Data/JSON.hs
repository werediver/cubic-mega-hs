module Local.Data.JSON where

import qualified Data.Map                      as Map

data JSON
  = JSONObject (Map.Map String JSON)
  | JSONArray [JSON]
  | JSONNumber Double
  | JSONString String
  | JSONBool Bool
  | JSONNull
  deriving (Show, Eq)

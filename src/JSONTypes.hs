module JSONTypes (JSONValue(..), JSONObject(..)) where

--  JSON-значение: строка, объект, массив
data JSONValue
  = JSONString String
  | JSONObj JSONObject
  | JSONArray [JSONValue]
  deriving (Show, Eq)

--  JSON-объект: список пар вида ключ-значение
newtype JSONObject = JSONObject [(String, JSONValue)]
  deriving (Show, Eq)
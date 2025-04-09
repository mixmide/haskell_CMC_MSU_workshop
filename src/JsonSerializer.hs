module JsonSerializer (jsonValueToString, jsonObjectToString) where

import JSONTypes
import Data.List (intercalate)


-- преобразоываем JSONValue в строку формата JSON
jsonValueToString :: JSONValue -> String
jsonValueToString (JSONString s) = "\"" ++ escapeString s ++ "\""
jsonValueToString (JSONObj obj) = jsonObjectToString obj
jsonValueToString (JSONArray arr) = "[" ++ intercalate ", " (map jsonValueToString arr) ++ "]"

-- преобразоываем JSONObject в строку
jsonObjectToString :: JSONObject -> String
jsonObjectToString (JSONObject []) = "{}"
jsonObjectToString (JSONObject pairs) =
  "{" ++ intercalate ", " [ "\"" ++ k ++ "\": " ++ jsonValueToString v | (k, v) <- pairs ] ++ "}"

-- показываем спец. символы в строке (уточнить)
escapeString :: String -> String
escapeString s = s
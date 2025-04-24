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

-- экранируем спец. символы в строке для JSON
escapeString :: String -> String
escapeString = concatMap escapeChar

escapeChar :: Char -> String
escapeChar c
  | c == '"'  = "\\\""
  | c == '\\' = "\\\\"
  | c == '\b' = "\\b"
  | c == '\f' = "\\f"
  | c == '\n' = "\\n"
  | c == '\r' = "\\r"
  | c == '\t' = "\\t"
  | c < ' '   = "\\u00" ++ toHex (fromEnum c)
  | otherwise = [c]

toHex :: Int -> String
toHex n = let hex = "0123456789ABCDEF"
          in [hex !! (n `div` 16), hex !! (n `mod` 16)]
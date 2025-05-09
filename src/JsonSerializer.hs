module JsonSerializer (jsonValueToString) where     -- на экспорт

import JSONTypes
import Data.List (intercalate) -- для объединения списка строк с разделителем, пример: intercalate ", " ["a", "b", "c"] -> "a, b, c"

-- преобразуем JSONValue в строку
jsonValueToString :: JSONValue -> String
jsonValueToString (JSONString s) =
  "\"" ++ escapeString s ++ "\""   -- добавляем кавычки в начало и конец, вызываем escapeString для экранирования спец. символов
jsonValueToString (JSONArray xs) =
  "[" ++ intercalate ", " (map jsonValueToString xs) ++ "]"      -- для случая массива
jsonValueToString (JSONObj (JSONObject pairs)) =                 -- pairs :: [(String, JSONValue)]
  "{"                                                            -- для обекта JSONObj 
  ++ intercalate ", "
       [ "\"" ++ k ++ "\":" ++ jsonValueToString v
       | (k, v) <- pairs
       ]
  ++ "}"

-- экранизация спецсимволов в СТРОКЕ
escapeString :: String -> String
escapeString = concatMap escapeChar -- применение escapeChar к каждому символу строки через Map, затем объединение через concat

escapeChar :: Char -> String        -- отдельно по символам
escapeChar c
  | c == '"'  = "\\\""  --      "   ->    \"
  | c == '\\' = "\\\\"  --      \   ->    \\
  | c == '\b' = "\\b"   -- Backspace
  | c == '\f' = "\\f"   -- Form feed
  | c == '\n' = "\\n"   -- новая строка
  | c == '\r' = "\\r"   -- возврат каретки
  | c == '\t' = "\\t"   -- табулция
  | c < ' '   = "\\u00" ++ toHex (fromEnum c) -- fromEnum для получения кода символа, toHex - перевод в 16-й формат => \u00AB (например)
  | otherwise = [c] -- иначе возвращает строку из одного символа (требуется сигнатурой функции)

toHex :: Int -> String  -- перевод числа в 16-ю СС
toHex n =
  let hex = "0123456789ABCDEF"
  in [ hex !! (n `div` 16)  -- !! - штука для доступа к эл-ту hex с индексом (n `div` 16)
     , hex !! (n `mod` 16)
     ]

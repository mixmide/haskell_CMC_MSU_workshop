module JSONTypes where

-- JSON-значение: строка, объект, массив (ПРОВЕРИТЬ)

data JSONValue
  = JSONString String        -- строка
  | JSONObject JSONObject    -- Объект
  | JSONArray [JSONValue]    -- массив значений
  deriving (Show, Eq)

-- JSON-объект - список пар "ключ-значение"

newtype JSONObject = JSONObject [(String, JSONValue)]
  deriving (Show, Eq)
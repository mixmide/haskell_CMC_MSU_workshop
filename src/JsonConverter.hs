module JsonConverter where

import XMLTypes
import JSONTypes


-- конвертация XMLNode в JSONValue
xmlToJson :: XMLNode -> JSONValue
xmlToJson = "help"      -- здесь будет рекурсивная конвертация узлов

-- Конвертирует атрибуты XML в JSON-объект
attrsToJson :: [(String, String)] -> JSONObject
attrsToJson = "help"    -- здесь будет преобразование атрибутов в пары "key": "value"
module JsonConverter (xmlToJsonRoot) where

import XMLTypes
import JSONTypes
-- импорты ниже нужны только для одного места
-- sortOn сортирует список элементов по ключу, вычисленному для каждого элемента
-- groupBy группирует последовательные элементы списка, которые удовлетворяют предикату
import Data.List (groupBy, sortOn)

-- конвертация XMLNode в JSONValue
xmlToJson :: XMLNode -> JSONValue
xmlToJson (TextNode s) = JSONString s
xmlToJson (ElementNode _ attrs children) =
  let
    -- separateChildren конкатенирует все строки из TextNode-ов и группирует оставшиеся элементные узлы по имени (для будущих массивов)
    -- groups — список пар (имя_тега, [XMLNode]) для каждого вида элемента, [XMLNode, ...] — список всех узлов с таким именем
    (text, groups) = separateChildren children

    -- каждый XML-атрибут key="value" превращается в JSON-поле "@key": "value"
    attrPairs  = [("@" ++ k, JSONString v) | (k, v) <- attrs]

    -- если  только текст без атрибутов и детей, просто возвращаем строку
    textPair   = if null text then [] else [("#text", JSONString text)]

    -- генерация списка, которая превращает каждый тег из groups в JSON-пару
    elemPairs  =
      [ (name, toJsonValue nodes)
      | (name, nodes) <- groups
      ]

    allPairs   = attrPairs ++ textPair ++ elemPairs -- объединение
  in
    case (attrs, groups, text) of
      ([], [], t) | not (null t) -> JSONString t   -- если только есть текст и нет ни атрибутов, ни дочерних элементов
      _                          -> JSONObj (JSONObject allPairs)

-- для корневого элемента: сохраняем имя тега
xmlToJsonRoot :: XMLNode -> JSONValue
xmlToJsonRoot node@(ElementNode name _ _) =
  JSONObj (JSONObject [(name, xmlToJson node)])
xmlToJsonRoot (TextNode s) =   -- обработка текстового случая (такого никогда не будет, но без него выдает ворнинг)
  JSONString s

-- при обработке групп элементов с одинаковыми именами
toJsonValue :: [XMLNode] -> JSONValue
toJsonValue [x] = xmlToJson x    -- если один элемент
toJsonValue xs  = JSONArray (map xmlToJson xs) -- если несколько

-- Функция разделяет список дочерних узлов (children) на 1) текстовую строку: объединяет все текстовые узлы в 1 строку и 2) группы элементов
separateChildren :: [XMLNode] -> (String, [(String, [XMLNode])])
separateChildren children =
  let
    -- собираем весь текст
    textElems  = [s | TextNode s <- children]
    text       = concat textElems

    -- оставляем только ElementNode
    elems      = [n | n@(ElementNode _ _ _) <- children]

    -- группируем по имени тега сохраняя исходный порядок элементов
    -- сортировка нужна ибо groupBy работает только с последовательными элементами
    -- groups     = groupBy ((==) `on` getName) (sortOn getName elems)
    groups     = groupBy sameName (sortOn getName elems) where
        sameName x y = getName x == getName y

    -- превращаем каждую непустую группу в пару (имя, список узлов с этим именем).
    keyed      = [ (getName first, grp) | grp@(first:_) <- groups ] -- сопоставление с образцом
  in
    (text, keyed)

-- извлекаем имя из ElementNode
getName :: XMLNode -> String
getName (ElementNode name _ _) = name
getName _ = error "Не удалось извлечь имя из ElementNode"
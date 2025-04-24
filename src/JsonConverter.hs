module JsonConverter (xmlToJson, xmlToJsonRoot) where

import XMLTypes
import JSONTypes
import Data.List (groupBy, sortBy)
import Data.Function (on)

--  конвертация XMLNode в JSONValue для дочерних элементов
xmlToJson :: XMLNode -> JSONValue
xmlToJson (TextNode s) = JSONString s
xmlToJson (ElementNode _ attrs children) =
  let (text, grouped) = separateChildren children
      attrPairs = [("@" ++ k, JSONString v) | (k, v) <- attrs]
      textPair = if null text then [] else [("#text", JSONString text)]
      elementPairs = [ (n, case nodes of
                       [single] -> xmlToJson single
                       _        -> JSONArray (map xmlToJson nodes))
               | (n, nodes) <- grouped ]
      allPairs = attrPairs ++ textPair ++ elementPairs
  in if null attrs && null grouped && not (null text)
     then JSONString text
     else JSONObj (JSONObject allPairs)

--  Конвертация корневого XMLNode в JSONValue (с оберткой имени тега)
xmlToJsonRoot :: XMLNode -> JSONValue
xmlToJsonRoot node@(ElementNode name _ _) =
  JSONObj (JSONObject [(name, xmlToJson node)])
xmlToJsonRoot (TextNode s) = JSONString s

-- делим дочерние узлы на текст и сгруппированные элементы
separateChildren :: [XMLNode] -> (String, [(String, [XMLNode])])
separateChildren children =
  let text = concat [s | TextNode s <- children]
      elements = [node | node@(ElementNode _ _ _) <- children]
      grouped = groupByName elements
  in (text, grouped)

-- группировка по имени
groupByName :: [XMLNode] -> [(String, [XMLNode])]
groupByName nodes =
  let sorted = sortBy (compare `on` getName) nodes
      grouped = groupBy ((==) `on` getName) sorted
  in map (\g -> case g of
                  (first:_) -> (getName first, g)
                  [] -> error "Группа пуста") grouped

getName :: XMLNode -> String  -- возвращает имя элементного узла
getName (ElementNode name _ _) = name
getName _ = error "Не элементный узел"
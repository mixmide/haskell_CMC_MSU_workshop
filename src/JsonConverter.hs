module JsonConverter (xmlToJson) where

import XMLTypes
import JSONTypes
import qualified Data.Map as Map

--   конвертация XMLNode в JSONValue
xmlToJson :: XMLNode -> JSONValue
xmlToJson (TextNode s) = JSONString s
xmlToJson (ElementNode _ attrs children) =
  let (text, grouped) = separateChildren children
      attrPairs = [("@" ++ k, JSONString v) | (k, v) <- attrs]
      textPair = if null text then [] else [("#text", JSONString text)]
      elementPairs = [ (n, case nodes of
                       [single] -> xmlToJson single
                       _        -> JSONArray (map xmlToJson nodes))
               | (n, nodes) <- Map.toList grouped ]
      allPairs = attrPairs ++ textPair ++ elementPairs
  in if null attrs && Map.null grouped && not (null text)
     then JSONString text
     else JSONObj (JSONObject allPairs)


-- здесь разделяем дочерние узлы на текст и сгруппированные элементы
separateChildren :: [XMLNode] -> (String, Map.Map String [XMLNode])
separateChildren children =
  let text = concat [s | TextNode s <- children]
      grouped = Map.fromListWith (++) [(name, [node]) | node@(ElementNode name _ _) <- children]
  in (text, grouped)

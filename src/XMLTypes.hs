module XMLTypes (XMLNode(..)) where


--  Узел XML: текстовый элемент или элемент с атрибутами и дочерними узлами
data XMLNode
  = TextNode String          -- текстовое содержимое, например text в <tag>text</tag>
  | ElementNode
      String                 -- название тега
      [(String, String)]     -- атрибуты (список пар типа "ключ-значение")
      [XMLNode]              -- дочерние узлы
  deriving (Show, Eq)
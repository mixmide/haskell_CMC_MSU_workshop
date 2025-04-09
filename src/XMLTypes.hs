module XMLTypes (XMLNode(..)) where

-- Узел XML: текстовый элемент или элемент с атрибутами и дочерними узлами
data XMLNode
  = TextNode String          -- текстовое содержимое (к примеру <tag>text</tag>)
  | ElementNode
      String                 -- название тега (к примеру "tag" в <tag>)
      [(String, String)]     -- атрибуты (список пар типа "ключ-значение")
      [XMLNode]              -- дочерние узлы
  deriving (Show, Eq)
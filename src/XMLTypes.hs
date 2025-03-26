module XMLTypes where

-- Узел XML. Для представления структуры XML-документа в виде дерева узлов

data XMLNode
  = TextNode String          -- Текстовый узел
  | ElementNode
      String                 -- Название тега
      [(String, String)]     -- Список пар "ключ-значение"
      [XMLNode]              -- Дочерние узлы
  deriving (Show, Eq)
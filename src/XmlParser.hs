module XmlParser where

import XMLTypes

-- Парсит строку в XML-структуру и возвращает либо строку с ошибкой или XMLNode
parseXml :: String -> Either String XMLNode
parseXml = "help"  -- здесь будет парсер
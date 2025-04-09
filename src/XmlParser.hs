module XmlParser (parseXml) where

import XMLTypes
import Data.Char (isSpace, isAlphaNum)



-- парсит строку в XML-структуру; возвращает Either с ошибкой или XMLNode
parseXml :: String -> Either String XMLNode
parseXml input =
  case parseElement (skipSpaces input) of
    Right (node, rest) ->
      if null (skipSpaces rest)
        then Right node
        else Left "Остаток после парсинга XML"
    Left err -> Left err

-- для пропуска пробелов
skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- парсит элемент XML
parseElement :: String -> Either String (XMLNode, String)
parseElement input =
  case input of
    ('<':rest) -> parseTag rest
    _ -> Left "Ожидался открывающий тег '<'"

-- парсит тег с его содержимым
parseTag :: String -> Either String (XMLNode, String)
parseTag input =
  case span isAlphaNum input of
    (name, rest) ->
      if null name
        then Left "Пустое имя тега"
        else
          let (attrs, rest') = parseAttributes (skipSpaces rest)
          in case skipSpaces rest' of
            ('>':content) -> parseContent name attrs content
            ('/':'>':rest'') -> Right (ElementNode name attrs [], rest'')
            _ -> Left "Ожидался '>' или '/>' после тега"

-- парсит атрибуты тега
parseAttributes :: String -> ([(String, String)], String)
parseAttributes input =
  case input of
    ('>':_) -> ([], input)
    ('/':'>':_) -> ([], input)
    _ ->
      case parseAttribute input of
        Just (attr, rest) ->
          let (attrs, rest') = parseAttributes (skipSpaces rest)
          in (attr : attrs, rest')
        Nothing -> ([], input)

-- парсит 1 атрибут
parseAttribute :: String -> Maybe ((String, String), String)
parseAttribute input =
  case span isAlphaNum input of
    (key, '=':'"':rest) ->
      case span (/= '"') rest of
        (value, '"':rest') -> Just ((key, value), rest')
        _ -> Nothing
    _ -> Nothing


-- парсит содержимое тега  (текст + дочерние элементы)
parseContent :: String -> [(String, String)] -> String -> Either String (XMLNode, String)
parseContent name attrs input =
  let (children, rest) = parseChildren input
  in case rest of
    ('<':'/':rest') ->
      case span isAlphaNum rest' of
        (closingName, '>':rest'') ->
          if closingName == name
            then Right (ElementNode name attrs children, rest'')
            else Left $ "Несоответствие закрывающего тега: ожидался " ++ name ++ ", но получен " ++ closingName
        _ -> Left "Некорректный закрывающий тег"
    _ -> Left "Ожидался закрывающий тег"

-- парсит дочерние узлы (текст или элементы)
parseChildren :: String -> ([XMLNode], String)
parseChildren input =
  case input of
    ('<':'/':_) -> ([], input)  -- т.е. конец содержимого
    ('<':_) ->
      case parseElement input of
        Right (node, rest) ->
          let (nodes, rest') = parseChildren rest
          in (node : nodes, rest')
        Left _ -> ([], input)
    _ ->
      let (text, rest) = span (/= '<') input
          trimmedText = skipSpaces text
      in if null trimmedText
           then parseChildren rest
           else let (nodes, rest') = parseChildren rest
                in (TextNode trimmedText : nodes, rest')
module XmlParser (parseXml) where

import XMLTypes -- типы данных из модуля XMLTypes.hs
import Data.Char (isSpace, isAlphaNum) -- для работы с символами
import Data.List (isPrefixOf, dropWhileEnd, isInfixOf) -- работа ср списками

-- чтение и удаление комментариев
skipComments :: String -> String
skipComments [] = []
skipComments s@('<':'!':'-':'-':_)
  | "<!--" `isPrefixOf` s =            -- доп. проверка
      let (_, after) = breakOn "-->" (drop 4 s)
      in case after of
           ('-':'-':'>':rest) -> skipComments rest
           _                  -> []  -- незакрытый комментарий
skipComments (c:cs) =
    c : skipComments cs

-- разбиение строки по первому вхождению какой-то строки (до и после)
breakOn :: String -> String -> (String, String)
breakOn pat = go []
  where
    go acc [] = (reverse acc, [])  -- go нужна для акума
    go acc xs@(y:ys)
      | pat `isPrefixOf` xs = (reverse acc, xs)
      | otherwise           = go (y:acc) ys

-- пропуск пробельных символов
skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- парсинг XML-дока
parseXml :: String -> Either String XMLNode
parseXml input =
    if "<!--" `isInfixOf` input && not ("-->" `isInfixOf` input)
      then Left "Незакрытый комментарий"
      else
        let cleaned = skipComments input
            input'  = skipSpaces cleaned
        in case input' of               -- обработка декларации
             ('<':'?':'x':'m':'l':rest) ->
               case span (/= '>') rest of
                 (_, '>':rest') -> parseXml rest'
                 _              -> Left "Некорректная декларация XML"
             _ -> case parseElement input' of  -- парсинг элемента
                    Right (node, rest)
                      | null (skipSpaces rest)
                      -> Right node
                      | otherwise
                      -> Left "Остаток после парсинга XML"
                    Left err -> Left err

-- парсинг одного XML-элемента
parseElement :: String -> Either String (XMLNode, String)
parseElement ('<':'/':_) =
    Left "Неожиданный закрывающий тег" -- ошибка

parseElement ('<':rest0) =          -- начало с открыв. тега 
    case span isAlphaNum rest0 of
      (name, rest1) ->
        let
            (attrs, rest2) = parseAttributes (skipSpaces rest1) -- извлекает список атрибутов (пример: key = "Value")
            restClean      = skipSpaces rest2
        in case restClean of

             -- самозакрывающийся тег <tag/>
             '/':'>':restAfter ->
                 Right (ElementNode name attrs [], restAfter)

             -- битый атрибут
             c:_
               | isAlphaNum c
               -> Left "Некорректный синтаксис атрибута"

             -- открывающий '>' и далее содержимое
             '>':content ->
               case parseChildren content of -- парсит дочерние узлы, результат сохр. в children
                 Right (children, afterChildren) ->
                   let rest4 = skipSpaces afterChildren
                   in case rest4 of
                        ('<':'/':closingRest) ->
                          case span isAlphaNum closingRest of  -- обработка закрываюшего тега
                            (closingName, '>':finalRest)
                              | closingName == name
                              -> Right (ElementNode name attrs children, finalRest)
                              | otherwise
                              -> Left
                                $ "Ожидался </" ++ name ++ ">, но получен </" ++ closingName ++ ">"
                            _ -> Left "Некорректный закрывающий тег"
                        _ ->
                          Left
                          $ "Ожидался закрывающий тег </" ++ name ++ ">, но получено: "
                          ++ if null rest4 then "<пусто>" else take 10 rest4
                 Left err -> Left err

             -- всё остальное
             _ ->
                 Left "Ожидался '>' после тега"

parseElement _ =
    Left "Ожидался открывающий тег '<'" -- некорректный вход

-- эта функция парсит атрибуты до '>' или '/>'
parseAttributes :: String -> ([(String, String)], String)
parseAttributes input =
    case input of
      ('>':_)     -> ([], input)
      ('/':'>':_) -> ([], input)
      _ ->
        case parseAttribute input of
          Just (attr, rest) ->
            let (attrs, rest') = parseAttributes (skipSpaces rest)
            in (attr : attrs, rest')
          Nothing -> ([], input)

-- эта функция парсит один атрибут
parseAttribute :: String -> Maybe ((String, String), String)
parseAttribute input =
    case span isAlphaNum input of
      (key, '=':'"':rest) ->
        case span (/= '"') rest of
          (value, '"':rest') -> Just ((key, value), rest')
          _                  -> Nothing
      _ -> Nothing

-- парсинг дочерних узлов
parseChildren :: String -> Either String ([XMLNode], String)
parseChildren input =
    let input' = skipSpaces input
    in if null input'
         then Right ([], "")
         else case input' of

                -- коммент внутри
                ('<':'!':'-':'-':_) ->
                  let (_, after) = breakOn "-->" (drop 4 input')
                  in parseChildren (drop 3 after)

                -- закрывающий тег: конец детей
                ('<':'/':_) -> Right ([], input')

                -- вложенный элемент
                ('<':_) ->
                  case parseElement input' of
                    Right (node, rest) ->
                      case parseChildren rest of
                        Right (nodes, rest') -> Right (node : nodes, rest')
                        Left err            -> Left err
                    Left err -> Left err

                -- текстовый узел
                _ ->
                  let (rawText, rest) = span (/= '<') input'
                      trimmed = dropWhileEnd isSpace (dropWhile isSpace rawText)
                  in if null trimmed
                        then parseChildren rest
                        else
                          case parseChildren rest of
                            Right (nodes, rest') -> Right (TextNode trimmed : nodes, rest')
                            Left err             -> Left err

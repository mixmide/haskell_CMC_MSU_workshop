module XmlParser (parseXml) where

import XMLTypes
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, dropWhileEnd, isInfixOf)

-- удаление комментариев XML вида <!-- ... -->
skipComments :: String -> String
skipComments [] = []
skipComments ('<':'!':'-':'-':cs) =
  case breakOn "-->" cs of
    (_, '-':'-':'>':xs) -> skipComments xs
    _                   -> []
skipComments (c:cs) = c : skipComments cs


-- разбиение строки по первому вхождению подстроки
breakOn :: String -> String -> (String, String)
breakOn pat = go []
  where
    go acc [] = (reverse acc, [])
    go acc xs@(y:ys)
      | pat `isPrefixOf` xs = (reverse acc, xs)
      | otherwise           = go (y:acc) ys

-- пропуск пробельных символов в начале строки
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
        in case input' of
             ('<':'?':'x':'m':'l':rest) ->
               case span (/= '>') rest of
                 (_, '>':rest') -> 
                    let afterParseXml = skipSpaces rest'
                    in case afterParseXml of
                        [] -> Left "Остаток после парсинга XML!"
                        _ -> case parseElement afterParseXml of
                                Right (node, rem') | null (skipSpaces rem') -> Right node
                                Right _ -> Left "Остаток после парсинга XML"
                                Left err -> Left err
                 _              -> Left "Некорректная декларация XML"
             _ -> case parseElement input' of
                    Right (node, rem')
                      | null (skipSpaces rem') -> Right node
                      | otherwise              -> Left "Остаток после парсинга XML"
                    Left err -> Left err

-- парсит XML-элемент
parseElement :: String -> Either String (XMLNode, String)
parseElement ('<':'/':_) =
    Left "Неожиданный закрывающий тег"
parseElement ('<':rest0) =
    case span isAlphaNum rest0 of
      (name, rest1) ->
        let (attrs, rest2)  = parseAttributes (skipSpaces rest1)
            restClean       = skipSpaces rest2
        in case restClean of

             -- самозакрывающийся тег <tag/>
             '/':'>':rem' ->
               Right (ElementNode name attrs [], rem')

             -- битый атрибут
             c:_
               | isAlphaNum c -> Left "Некорректный синтаксис атрибута"

             -- открывающий '>' и далее содержимое
             '>':content ->
               case parseChildren content of
                 Right (children, after) ->
                   let rest4 = skipSpaces after
                   in case rest4 of
                        ('<':'/':closing) ->
                          case span isAlphaNum closing of
                            (cn, '>':final)
                              | cn == name -> Right (ElementNode name attrs children, final)
                              | otherwise  -> Left $
                                  "Ожидался </" ++ name ++ ">, но получен </" ++ cn ++ ">"
                            _ -> Left "Некорректный закрывающий тег"
                        _ ->
                          Left $
                            "Ожидался закрывающий тег </" ++ name ++ ">, но получено: "
                            ++ if null rest4 then "<пусто>" else take 10 rest4
                 Left err -> Left err

             -- всё остальное: ожидался '>' или '/>'
             _ -> Left "Ожидался '>' после тега"
parseElement _ =
    Left "Ожидался открывающий тег '<'"


-- парсит все атрибуты (до '>' или '/>')
parseAttributes :: String -> ([(String, String)], String)
parseAttributes input =
    case input of
      ('>':_)     -> ([], input)
      ('/':'>':_) -> ([], input)
      _ -> case parseAttribute input of
             Just (attr, rem') ->
               let (attrs, rem'') = parseAttributes (skipSpaces rem')
               in (attr : attrs, rem'')
             Nothing -> ([], input)

-- парсит один атрибут (key="value")
parseAttribute :: String -> Maybe ((String, String), String)
parseAttribute input =
    case span isAlphaNum input of
      (key, '=':'"':rest) ->
        case span (/= '"') rest of
          (val, '"':rem') -> Just ((key, val), rem')
          _               -> Nothing
      _ -> Nothing

-- Парсит дочерние узлы и возвращает либо ошибку, либо список и остаток
parseChildren :: String -> Either String ([XMLNode], String)
parseChildren input =
    let input' = skipSpaces input
    in if null input'
         then Right ([], "")
         else case input' of

                -- закрывающий тег: конец списка детей
                ('<':'/':_) -> Right ([], input')

                -- вложенный элемент
                ('<':_) ->
                  case parseElement input' of
                    Right (node, rem') ->
                      case parseChildren rem' of
                        Right (nodes, rem'') -> Right (node : nodes, rem'')
                        Left err            -> Left err
                    Left err -> Left err

                -- текстовый узел
                _ ->
                  let (raw, rem') = span (/= '<') input'
                      trimmed     = dropWhileEnd isSpace (dropWhile isSpace raw)
                  in if null trimmed
                        then parseChildren rem'
                        else case parseChildren rem' of
                               Right (nodes, rem'') -> Right (TextNode trimmed : nodes, rem'')
                               Left err             -> Left err

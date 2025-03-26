module Main where

import System.Environment (getArgs)
import XmlParser (parseXml)
import JsonConverter (xmlToJson)
import JsonSerializer (jsonValueToString)



main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs         -- получает аргументы из кмд строки
  xmlContent <- readFile inputFile           -- читает XML-файл
  case parseXml xmlContent of
    Left err -> putStrLn $ "Ошибка парсинга: " ++ err
    Right xmlNode -> do
      let jsonValue = xmlToJson xmlNode                        -- конвертирует в JSON
      writeFile outputFile (jsonValueToString jsonValue)       -- здесь будет лежать результат
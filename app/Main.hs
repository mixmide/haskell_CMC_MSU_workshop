module Main (main) where

import System.Environment (getArgs)
import XmlParser       (parseXml)
import JsonConverter   (xmlToJsonRoot)
import JsonSerializer  (jsonValueToString)

main :: IO ()
main = do
  args <- getArgs -- cчитывает аргументы кмд строки
  case args of
    [inputFile, outputFile] -> do  -- проверка на корректность аргументов кмд строки
      content <- readFile inputFile -- считывает содержимое XML-файла в строку
      case parseXml content of
        Left err      -> putStrLn $ "Ошибка парсинга: " ++ err
        Right xmlTree -> 
          writeFile outputFile (jsonValueToString (xmlToJsonRoot xmlTree))
    _ -> 
      putStrLn "Использование: data-project-exe <input.xml> <output.json>"
module Main (main) where

import System.Environment (getArgs)
import XmlParser (parseXml)
import JsonConverter (xmlToJson)
import JsonSerializer (jsonValueToString)


main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs
  xmlContent <- readFile inputFile
  case parseXml xmlContent of
    Left err -> putStrLn $ "Ошибка парсинга: " ++ err
    Right xmlNode -> do
      let jsonValue = xmlToJson xmlNode
      writeFile outputFile (jsonValueToString jsonValue)
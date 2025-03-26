module Main where

import System.Environment (getArgs)
import XmlParser (parseXml)
import JsonConverter (xmlToJson)
import JsonSerializer (jsonValueToString)



main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs         -- �������� ��������� �� ��� ������
  xmlContent <- readFile inputFile           -- ������ XML-����
  case parseXml xmlContent of
    Left err -> putStrLn $ "������ ��������: " ++ err
    Right xmlNode -> do
      let jsonValue = xmlToJson xmlNode                        -- ������������ � JSON
      writeFile outputFile (jsonValueToString jsonValue)       -- ����� ����� ������ ���������
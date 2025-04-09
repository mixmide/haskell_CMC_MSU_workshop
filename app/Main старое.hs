module Main where

import System.Environment (getArgs)
import XmlParser (parseXml)
import JsonConverter (xmlToJson)
import JsonSerializer (jsonValueToString)



main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs         -- ïîëó÷àåò àðãóìåíòû èç êìä ñòðîêè
  xmlContent <- readFile inputFile           -- ÷èòàåò XML-ôàéë
  case parseXml xmlContent of
    Left err -> putStrLn $ "Îøèáêà ïàðñèíãà: " ++ err
    Right xmlNode -> do
      let jsonValue = xmlToJson xmlNode                        -- êîíâåðòèðóåò â JSON
      writeFile outputFile (jsonValueToString jsonValue)       -- çäåñü áóäåò ëåæàòü ðåçóëüòàò

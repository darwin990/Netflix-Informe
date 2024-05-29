module Main where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import ETL (ShowInfo)

main :: IO ()
main = do
  jsonContent <- B.readFile "data/netflix_titles_nov_2019.json"
  let maybeShowInfo = decode jsonContent :: Maybe ShowInfo
  case maybeShowInfo of
    Just showInfo -> print showInfo
    Nothing -> putStrLn "No se pudo decodificar el JSON."

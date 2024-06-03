{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

-- Definimos un tipo de dato para representar cada fila del CSV.
data ShowInfo = ShowInfo
  { showId :: !String
  , title :: !String
  , director :: !(Maybe String)
  , cast :: !(Maybe String)
  , country :: !String
  , dateAdded :: !String
  , releaseYear :: !Int
  , rating :: !String
  , duration :: !String
  , listedIn :: !String
  , description :: !String
  , type' :: !String
  } deriving (Show)

-- Instancia para convertir cada fila del CSV a nuestro tipo de dato.
instance FromNamedRecord ShowInfo where
  parseNamedRecord r = ShowInfo <$> r .: "show_id"
                                 <*> r .: "title"
                                 <*> r .: "director"
                                 <*> r .: "cast"
                                 <*> r .: "country"
                                 <*> r .: "date_added"
                                 <*> r .: "release_year"
                                 <*> r .: "rating"
                                 <*> r .: "duration"
                                 <*> r .: "listed_in"
                                 <*> r .: "description"
                                 <*> r .: "type"

main :: IO ()
main = do
  -- Leemos el contenido del archivo CSV.
  csvData <- BL.readFile "data/netflix_titles_nov_2019.csv"
  -- Decodificamos el contenido como un CSV con cabecera.
  case decodeByName csvData :: Either String (Header, V.Vector ShowInfo) of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ \showInfo ->
      -- Aquí puedes trabajar con cada 'showInfo' que es una fila del CSV.
      print showInfo

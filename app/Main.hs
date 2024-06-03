{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.List (sortBy) -- Importamos sortBy de Data.List
import Data.Ord (comparing) -- Importamos comparing de Data.Ord

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

-- Función para extraer el valor numérico de la duración.
-- Función para extraer el valor numérico de la duración.
extractDuration :: String -> Int
extractDuration s = read $ filter isDigit s

main :: IO ()
main = do
  -- Leemos el contenido del archivo CSV.
  csvData <- BL.readFile "data/netflix_titles_nov_2019.csv"
  -- Decodificamos el contenido como un CSV con cabecera.
  case decodeByName csvData :: Either String (Header, V.Vector ShowInfo) of
    Left err -> putStrLn err
    Right (_, v) -> do
      -- Utilizamos un Map para contar las ocurrencias de cada país.
      let countryCounts = V.foldl' (\acc showInfo ->
            let countries = splitOn ", " (country showInfo) -- Separamos los países por comas.
            in foldr (\c -> Map.insertWith (+) c 1) acc countries) Map.empty v
      -- Contamos los tipos de shows.
      let typeCounts = V.foldl' (\acc showInfo -> Map.insertWith (+) (type' showInfo) 1 acc) Map.empty v
      -- Filtramos y ordenamos las películas por duración.
      let movies = V.filter (\showInfo -> type' showInfo == "Movie") v
      let sortedMovies = sortBy (comparing (negate . extractDuration . duration)) (V.toList movies)
      let top10Movies = take 10 sortedMovies
      -- Imprimimos el resultado.
      putStrLn "Conteo de países:"
      mapM_ (putStrLn . (\(k, v) -> k ++ ": " ++ show v)) (Map.toList countryCounts)
      putStrLn "\nConteo de tipos:"
      mapM_ (putStrLn . (\(k, v) -> k ++ ": " ++ show v)) (Map.toList typeCounts)
      putStrLn "\nTop 10 películas por duración:"
      mapM_ (putStrLn . (\showInfo -> title showInfo ++ " - " ++ duration showInfo)) top10Movies
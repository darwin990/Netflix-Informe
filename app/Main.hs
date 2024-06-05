
-- Programa para analisis de las series y peliculas de Netflix hasta el 2021
-- Participantes:
-- Nestor Alejandro Morales Martinez - 20212020065
-- Martin Elias Cudris Aguilar - 20212020066

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding ((.=))
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)
import Data.List (sortBy) -- Importamos sortBy de Data.List
import Data.Ord (comparing) -- Importamos comparing de Data.Ord
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)

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

-- Función para generar el diagrama circular con los 10 países más frecuentes (Despues de una hora de ejecución no soltó nada).
generatePieChart :: [(String, Int)] -> IO ()
generatePieChart top10Countries = toFile def "country_pie_chart.svg" $ do
    pie_title .= "Top 10 Países Más Frecuentes"
    pie_plot . pie_data .= map (\(country, count) -> PieItem country (fromIntegral count) 0.0) top10Countries
    pie_plot . pie_colors .= cycle [opaque blue, opaque green, opaque red, opaque yellow]
    pie_plot . pie_start_angle .= 180


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
            case country showInfo of
              "" -> acc -- Ignoramos las entradas sin país
              c -> Map.insertWith (+) c 1 acc) Map.empty v

      -- Contamos los tipos de shows.
      let typeCounts = V.foldl' (\acc showInfo -> Map.insertWith (+) (type' showInfo) 1 acc) Map.empty v

      -- Filtramos y ordenamos las películas por duración.
      let movies = V.filter (\showInfo -> type' showInfo == "Movie") v
      let sortedMovies = sortBy (comparing (negate . extractDuration . duration)) (V.toList movies) -- Ordenamos segun las duraciones más altas
      let top10Movies = take 10 sortedMovies -- Tomamos los primeros 10 resultados

      -- Imprimimos el resultado de los 10 países más frecuentes.
      putStrLn "Top 10 países más frecuentes:"
      let top10Countries = take 10 . sortBy (comparing (negate . snd)) . Map.toList $ countryCounts -- Tomamos los primeros 10 resultados
      mapM_ (putStrLn . (\(k, v) -> k ++ ": " ++ show v)) top10Countries

      -- Imprimimos el resultado de cuantas peliculas y series hay.
      putStrLn "\nConteo de tipos:" 
      mapM_ (putStrLn . (\(k, v) -> k ++ ": " ++ show v)) (Map.toList typeCounts)

      -- Imprimimos el resultado de cuantas peliculas y series hay.
      putStrLn "\nTop 10 películas por duración:"
      mapM_ (putStrLn . (\showInfo -> title showInfo ++ " - " ++ duration showInfo)) top10Movies
      
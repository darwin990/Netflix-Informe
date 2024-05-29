{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ETL (ShowInfo) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ShowInfo = ShowInfo
  { show_id      :: Int
  , title        :: String
  , director     :: String
  , cast         :: String
  , country      :: String
  , date_added   :: String
  , release_year :: Int
  , rating       :: String
  , duration     :: String
  , listed_in    :: String
  , description  :: String
  , type'        :: String
  } deriving (Show, Generic)

instance FromJSON ShowInfo where
  parseJSON = withObject "ShowInfo" $ \v -> ShowInfo
    <$> v .: "show_id"
    <*> v .: "title"
    <*> v .: "director"
    <*> v .: "cast"
    <*> v .: "country"
    <*> v .: "date_added"
    <*> v .: "release_year"
    <*> v .: "rating"
    <*> v .: "duration"
    <*> v .: "listed_in"
    <*> v .: "description"
    <*> v .: "type"

instance ToJSON ShowInfo where
  toJSON (ShowInfo show_id title director cast country date_added release_year rating duration listed_in description type') =
    object [ "show_id"      .= show_id
           , "title"        .= title
           , "director"     .= director
           , "cast"         .= cast
           , "country"      .= country
           , "date_added"   .= date_added
           , "release_year" .= release_year
           , "rating"       .= rating
           , "duration"     .= duration
           , "listed_in"    .= listed_in
           , "description"  .= description
           , "type"         .= type'
           ]

{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Aeson
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Data.Text (Text)
import Data.Vector

data ImageCreateRequest
  = ImageCreateRequest
      { imageUrl :: Text,
        imageLabel :: Maybe Text,
        enableObjectDetection :: Bool
      }
  deriving (Show, Generic)

instance FromJSON ImageCreateRequest


data Image
    = Image
      {
        id :: Int,
        url :: Text,
        label :: Text,
        objects :: Vector Text
      }
  deriving (Generic, Show)

instance ToJSON Image
instance ToRow Image
instance FromRow Image

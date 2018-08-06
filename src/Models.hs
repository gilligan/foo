{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Models where

import qualified Data.Text as T

import Data.Aeson
import GHC.Generics


data Airport = Airport { iataCode :: T.Text
                       , name :: T.Text
                       } deriving (Eq, Show, Generic)

instance ToJSON Airport
instance FromJSON Airport

data Timezone = Timezone { iataCode :: T.Text
                         , timezone :: T.Text
                         } deriving (Eq, Show, Generic)

instance ToJSON Timezone
instance FromJSON Timezone


newtype  Health = Health T.Text
    deriving (Eq, Show, Generic)

instance ToJSON Health
instance FromJSON Health


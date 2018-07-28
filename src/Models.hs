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

newtype  Health = Health T.Text
    deriving (Eq, Show, Generic)

instance ToJSON Health
instance FromJSON Health


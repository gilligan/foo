{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (
      Config (..)
    , loadConfig
    ) where

import Control.Exception
import Data.Aeson
import GHC.Generics
import System.IO.Error

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

configFilePath :: String
configFilePath = "./config.json"

data Config = Config {
                       appPort  :: Int
                     , mongoUri :: T.Text
                     , gracePeriodSec :: Integer
                     } deriving (Eq, Show, Generic)

instance ToJSON Config
instance FromJSON Config

safeRead :: FilePath -> IO (Maybe BS.ByteString)
safeRead path = fmap Just (BS.readFile path) `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

loadConfig :: IO (Maybe Config)
loadConfig = do
    x <- safeRead configFilePath
    case x of
        Just xs -> return (decode xs)
        Nothing -> return Nothing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Airport (
      AirportApi
    , airportApi
    , airportServer
    ) where

import Control.Monad.Reader (asks)
import Control.Monad.Except  (MonadIO, liftIO)

import qualified Data.Text as T
import Servant

import Types
import Models
import Db

type AirportApi = "airports" :> QueryParam "iata" T.Text :> Get '[JSON] [Airport]

airportApi :: Proxy AirportApi
airportApi = Proxy

airportServer :: MonadIO m => ServerT AirportApi (AppT m)
airportServer = listAirports

listAirports :: MonadIO m => Maybe T.Text -> AppT m [Airport]
listAirports (Just iata) = getAirports [ "iataCode" =: iata ]
listAirports Nothing     = getAirports [ ]

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
import Config

type AirportApi = "airports" :> QueryParam "iata" T.Text :> Get '[JSON] [Airport]

airportApi :: Proxy AirportApi
airportApi = Proxy

airportServer :: MonadIO m => ServerT AirportApi (AppT m)
airportServer = listAirports

airports :: [Airport]
airports = [ Airport "HAM" "Hamburg"
               , Airport "BER" "Berlin"
               , Airport "ZRH" "Zurich"
               , Airport "MUC" "Munich"
               ]


listAirports :: MonadIO m => Maybe T.Text -> AppT m [Airport]
listAirports (Just iata) = asks dbConn >>= \conn -> liftIO $ getAirports conn [ "iataCode" =: iata ]
listAirports Nothing     = asks dbConn >>= \conn -> liftIO $ getAirports conn [ ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Airport where

import qualified Data.Text as T
import Servant

import Models

airports :: [Airport]
airports = [ Airport "HAM" "Hamburg"
               , Airport "BER" "Berlin"
               , Airport "ZRH" "Zurich"
               , Airport "MUC" "Munich"
               ]

type AirportApi =
    "airports"    :> QueryParam "iata" T.Text :> Get '[JSON] [Airport] :<|>
    "healthcheck" :> Get '[JSON] Health

server :: Server AirportApi
server =
    listAirports :<|>
    healthCheck

healthCheck :: Handler Health
healthCheck = return $ Health (T.pack "OK")

listAirports :: Maybe T.Text -> Handler [Airport]
listAirports Nothing     = return airports
listAirports (Just iata) = return $ filter (\a -> iataCode a == iata) airports

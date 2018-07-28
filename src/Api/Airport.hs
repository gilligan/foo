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

type AirportApi = "airports" :> QueryParam "iata" T.Text :> Get '[JSON] [Airport]

airportServer :: Server AirportApi
airportServer = listAirports

listAirports :: Maybe T.Text -> Handler [Airport]
listAirports Nothing     = return airports
listAirports (Just iata) = return $ filter (\a -> iataCode a == iata) airports

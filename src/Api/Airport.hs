{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Airport (
      AirportApi
    , airportApi
    , airportServer
    ) where


import           Control.Monad.Except (MonadIO)
import qualified Data.Text as T
import           Servant

import Types
import Models (Airport, Timezone)
import Db (Selector, getEntity, (=:))

type AirportApi = "airports" :> QueryParam "iata" T.Text :> Get '[JSON] [Airport]
               :<|> "timezone" :> QueryParam "iata" T.Text :> Get '[JSON] [Timezone]

airportApi :: Proxy AirportApi
airportApi = Proxy

airportServer :: MonadIO m => ServerT AirportApi (AppT m)
airportServer = listAirports :<|> listTimezones

selectByIataCode :: T.Text -> Selector
selectByIataCode xs = [ "$or" =: (\x -> [ "iataCode" =: x ]) <$> T.splitOn "," xs ]

listAirports :: MonadIO m => Maybe T.Text -> AppT m [Airport]
listAirports Nothing     = getEntity [ ] 
listAirports (Just iata) = getEntity $ selectByIataCode iata

listTimezones :: MonadIO m => Maybe T.Text -> AppT m [Timezone]
listTimezones Nothing     = getEntity [ ]
listTimezones (Just iata) = getEntity $ selectByIataCode iata

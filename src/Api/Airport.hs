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
import Models
import Db

type AirportApi = "airports" :> QueryParam "iata" T.Text :> Get '[JSON] [Airport]
               :<|> "timezone" :> QueryParam "iata" T.Text :> Get '[JSON] [Timezone]

airportApi :: Proxy AirportApi
airportApi = Proxy

airportServer :: MonadIO m => ServerT AirportApi (AppT m)
airportServer = listAirports :<|> listTimezones

listAirports :: MonadIO m => Maybe T.Text -> AppT m [Airport]
listAirports (Just iata) = getEntity [ "iataCode" =: iata ]
listAirports Nothing     = getEntity [ ] 

listTimezones :: MonadIO m => Maybe T.Text -> AppT m [Timezone]
listTimezones (Just iata) = getEntity [ "iataCode" =: iata ]
listTimezones Nothing     = getEntity [ ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib  where

import qualified Data.Text as T

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

airports :: [Airport]
airports = [  Airport (T.pack "HAM") (T.pack "Hamburg")
           , Airport (T.pack "BER") (T.pack "Berlin")
           , Airport (T.pack "ZRH") (T.pack "Zurich")
           , Airport (T.pack "MUC") (T.pack "Munich")
           ]

data Airport = Airport { iataCode :: T.Text
                       , name :: T.Text
                       } deriving (Eq, Show, Generic)

instance ToJSON Airport
instance FromJSON Airport

newtype  Health = Health T.Text
    deriving (Eq, Show, Generic)

instance ToJSON Health
instance FromJSON Health

type AirportApi =
    "airports"    :> QueryParam "iata" T.Text :> Get '[JSON] [Airport] :<|>
    "healthcheck" :> Get '[JSON] Health


airportApi :: Proxy AirportApi
airportApi = Proxy

runApp :: IO ()
runApp = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

server :: Server AirportApi
server =
    listAirports :<|>
    healthCheck


mkApp :: IO Application
mkApp = return $ serve airportApi server

healthCheck :: Handler Health
healthCheck = return $ Health (T.pack "OK")

listAirports :: Maybe T.Text -> Handler [Airport]
listAirports Nothing     = return airports
listAirports (Just iata) = return $ filter (\a -> iataCode a == iata) airports

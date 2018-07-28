{-# LANGUAGE TypeOperators #-}

module Api (mkApp)  where

import qualified Data.Text as T

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Models
import Api.Airport
import Api.Health

type AppAPI = AirportApi :<|> HealthApi

appAPI :: Proxy AppAPI
appAPI = Proxy

mkApp :: IO Application
mkApp = return $ serve appAPI $ airportServer :<|> healthServer

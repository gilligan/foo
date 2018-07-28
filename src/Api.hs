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

airportApi :: Proxy AirportApi
airportApi = Proxy

mkApp :: IO Application
mkApp = return $ serve airportApi server

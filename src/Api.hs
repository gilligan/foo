{-# LANGUAGE TypeOperators #-}

module Api (mkApp)  where

import qualified Data.Text as T

import Control.Monad.Reader (runReaderT)
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Types
import Config
import Models
import Api.Airport
import Api.Health

type AppApi = AirportApi :<|> HealthApi


appAPI :: Proxy AppApi
appAPI = Proxy

-- | Creates a Server for the airport API running in our AppT transformer
appToServer :: Config -> Server AirportApi
appToServer cfg = hoistServer airportApi (convertApp cfg) airportServer

-- | Converts a handler running in (AppT IO) into
--   (Handler a) as expected by Servant
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg app = Handler $ runReaderT (runApp app) cfg

-- | Take a configuration and return an Application that can be 
-- server by WAI.
mkApp :: Config -> Application
mkApp cfg = serve appAPI (appToServer cfg :<|> healthServer)

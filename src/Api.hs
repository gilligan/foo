{-# LANGUAGE TypeOperators #-}

module Api (mkApp)  where

import           Control.Monad.Reader (runReaderT)
import           Servant

import Types        (AppCtx(..), AppT(..))
import Api.Airport
import Api.Health

type AppApi = AirportApi :<|> HealthApi

appAPI :: Proxy AppApi
appAPI = Proxy

-- | Creates a Server for the airport API running in our AppT transformer
appToServer :: AppCtx -> Server AirportApi
appToServer ctx = hoistServer airportApi (convertApp ctx) airportServer

-- | Converts a handler running in (AppT IO) into
--   (Handler a) as expected by Servant
convertApp :: AppCtx -> AppT IO a -> Handler a
convertApp ctx app = Handler $ runReaderT (runApp app) ctx

-- | Take a configuration and return an Application that can be 
-- server by WAI.
mkApp :: AppCtx -> Application
mkApp ctx = serve appAPI (appToServer ctx :<|> healthServer)

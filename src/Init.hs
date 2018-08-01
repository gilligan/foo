{-# LANGUAGE OverloadedStrings #-}

module Init (startApp) where

import Control.Monad
import Data.Maybe

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import System.Posix.Env
import Safe (readMay)

import Types (Config(..))
import Api
import Db (getConnection)

startApp :: IO ()
startApp = do
    cfg <- initConfig
    when (isNothing cfg) (error "No configuration found. Exiting")
    runWithConfig (fromJust cfg)

runWithConfig :: Config -> IO ()
runWithConfig cfg = runSettings settings (mkApp cfg)
    where
        port = appPort cfg
        settings = setPort port $
            setBeforeMainLoop (hPutStrLn stderr $ "Listening on port " ++ show port)
            defaultSettings

initConfig :: IO (Maybe Config)
initConfig = do
        port      <- readMay <$> getEnvDefault "APP_PORT" "3000" 
        mongoHost <- getEnvDefault "MONGO_URI" "127.0.0.1" 
        graceSecs <- readMay <$> getEnvDefault "GRACE_PERIOD_SEC" "5000" 
        conn      <- getConnection mongoHost

        return $ Config <$> port
                        <*> Just mongoHost
                        <*> graceSecs
                        <*> Just conn


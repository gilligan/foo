{-# LANGUAGE OverloadedStrings #-}

module Init (startApp) where

import Control.Monad               (when)
import Data.Maybe                  (isNothing, fromJust)
import Network.Wai.Handler.Warp    (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Safe                        (readMay)
import System.Exit                 (exitFailure)
import System.IO                   (hPutStrLn, stderr)
import System.Posix.Env            (getEnvDefault)

import Api   (mkApp)
import Db    (getConnection)
import Types (Config(..))

startApp :: IO ()
startApp = do
    cfg <- initConfig
    when (isNothing cfg) (putStrLn "No configuration found. Exiting" >> exitFailure)
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
                        <*> conn


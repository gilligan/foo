{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Init (startApp) where

import Network.Wai.Handler.Warp    (defaultSettings, runSettings, setBeforeMainLoop, setPort, setLogger)
import Safe                        (readMay)
import System.Exit                 (exitFailure)
import System.IO                   (hPutStrLn, stderr)
import System.Posix.Env            (getEnv)
import Control.Monad.Except
import Network.Wai.Logger          (withStdoutLogger)

import Api   (mkApp)
import Db    (getConnection)
import Types (Config(..), InitError(..))

startApp :: IO ()
startApp = initConfig >>= \case
    Right c -> runWithConfig c
    Left  err -> putStrLn (reportError err) >> exitFailure

reportError :: InitError -> String
reportError ConfigErrorPort = "Error: service port not configured. Please set the APP_PORT environment variable"
reportError ConfigErrorHost = "Error: mongo host is not configured. Please set the MONGO_URI environment variable"
reportError InitErrorDbConnection = "Error: failed to connect to mongo database"
reportError ConfigErrorGraceSecs = "Error: grace shutdown timeout not configured. Please set the GRACE_PERIOD_SEC environment variable"
    
runWithConfig :: Config -> IO ()
runWithConfig cfg =
    withStdoutLogger $ \logger -> do

        let settings = setPort (appPort cfg) 
                    $ setBeforeMainLoop (hPutStrLn stderr $ "Starting up service on port " ++ show (appPort cfg))
                    $ setLogger logger defaultSettings

        runSettings settings (mkApp cfg)


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

getEnvWith :: (String -> Maybe b) -> a -> String -> ExceptT a IO b
getEnvWith readF err str =  liftIO (getEnv str) >>= \case
  Just val -> liftEither $ maybeToEither err (readF val)
  Nothing  -> throwError err

readEnv :: Read b => a -> String -> ExceptT a IO b
readEnv = getEnvWith readMay

readEnvStr :: a -> String -> ExceptT a IO String
readEnvStr = getEnvWith Just

initConfig :: IO (Either InitError Config)
initConfig = runExceptT $ do
    port <- readEnv ConfigErrorPort "APP_PORT"
    mongoHost <- readEnvStr ConfigErrorHost "MONGO_URI"
    graceSecs <- readEnv ConfigErrorGraceSecs "GRACE_PERIOD_SEC"

    connection <- ExceptT $ getConnection mongoHost
    return $ Config port mongoHost graceSecs connection


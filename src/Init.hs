{-# LANGUAGE OverloadedStrings #-}

module Init (startApp) where

import Network.Wai.Handler.Warp    (defaultSettings, runSettings, setBeforeMainLoop, setPort, setLogger)
import Safe                        (readMay)
import System.Exit                 (exitFailure)
import System.IO                   (hPutStrLn, stderr)
import System.Posix.Env            (getEnv)

import           Network.Wai.Logger                     (withStdoutLogger)

import Api   (mkApp)
import Db    (getConnection)
import Types (Config(..))

startApp :: IO ()
startApp = do
    cfg <- initConfig
    case cfg of
        Right c -> runWithConfig c
        Left  err -> reportError err >> exitFailure

reportError :: InitError -> IO ()
reportError = putStr . show
    
runWithConfig :: Config -> IO ()
runWithConfig cfg =
    withStdoutLogger $ \logger -> do

        let settings = setPort (appPort cfg) 
                    $ setBeforeMainLoop (hPutStrLn stderr $ "Starting up service on port " ++ show (appPort cfg))
                    $ setLogger logger defaultSettings

        runSettings settings (mkApp cfg)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

readEnv :: Read b => a -> String -> IO (Either a b)
readEnv err str = do
    x <- getEnv str
    return $ case x of
        (Just val) -> maybeToEither err (readMay val)
        Nothing    -> Left err

data InitError = ConfigErrorPort
               | ConfigErrorHost
               | ConfigErrorGraceSecs
               | InitErrorDbConnection
               deriving (Show, Eq)
              

initConfig :: IO (Either InitError Config)
initConfig = do
    port <- readEnv ConfigErrorPort "APP_PORT"
    mongoHost <- readEnv ConfigErrorHost "MONGO_URI"
    graceSecs <- readEnv ConfigErrorGraceSecs "GRACE_PERIOD_SEC"
    
    conn <- case mongoHost of
        Right h -> getConnection InitErrorDbConnection h
        Left  x -> return $ Left x

    return $ Config <$> port
                    <*> mongoHost
                    <*> graceSecs
                    <*> conn

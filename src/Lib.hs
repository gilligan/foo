{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib (runApp) where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

data Foo = Foo { fooId :: Integer
               , fooText :: String
               } 
               deriving (Eq, Show, Generic)

instance ToJSON Foo
instance FromJSON Foo

type FooApi = "foo" :> Get '[JSON] [Foo]

fooApi :: Proxy FooApi
fooApi = Proxy

runApp :: IO ()
runApp = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

server :: Server FooApi
server = getFoos

mkApp :: IO Application
mkApp = return $ serve fooApi server

getFoos :: Handler [Foo]
getFoos = return [ Foo 0 "first foo"
                 , Foo 1 "second foo"
                 , Foo 2 "third foo"
                 , Foo 3 "fourth foo"
                 ]

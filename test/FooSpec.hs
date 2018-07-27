module FooSpec where

import           Control.Exception (throwIO)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           Lib hiding (getFoos)

getFoos :: ClientM [Foo]
getFoo :: Integer -> ClientM Foo
getFoos :<|> getFoo = client fooApi

spec :: Spec
spec = do
  describe "/item" $ do
    withClient mkApp $ do
      it "lists all items" $ \ env -> do
        try env getFoos `shouldReturn` [ Foo 0 "first foo"
                 , Foo 1 "second foo"
                 , Foo 2 "third foo"
                 , Foo 3 "fourth foo"
                 ]

      it "allows to show items by id" $ \ env -> do
        try env (getFoo 0) `shouldReturn` Foo 0 "0"


withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv manager baseUrl)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv

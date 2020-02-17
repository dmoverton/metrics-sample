{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( startApp
  , app
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (forM_, forever)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                     (pack)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.WebSockets.Connection (Connection, forkPingThread,
                                                sendTextData)
import           Servant
import           Servant.API.WebSocket         (WebSocket (..))
import qualified System.Metrics                as Metrics

data User =
  User
    { userId        :: Int
    , userFirstName :: String
    , userLastName  :: String
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type WebSocketApi = "health" :> WebSocket

type API = "users" :> Get '[ JSON] [User] :<|> WebSocketApi

newtype Env =
  Env
    { _metricsStore :: Metrics.Store
    }

startApp :: IO ()
startApp = do
  metricsStore <- Metrics.newStore
  Metrics.registerGcMetrics metricsStore
  let env = Env metricsStore
  run 8080 $ app env

app :: Env -> Application
app = serve api . server

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = return users :<|> wsServer env

users :: [User]
users = [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]

wsServer :: Env -> Server WebSocketApi
wsServer env = streamData
  where
    streamData :: MonadIO m => Connection -> m ()
    streamData c = do
      liftIO $ forkPingThread c 10
      liftIO . forever $ do
        sample <- Metrics.sampleAll $ _metricsStore env
        sendTextData c (pack $ show sample) >> threadDelay 1000000

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (pack)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection (Connection, forkPingThread, sendTextData)
import Servant
import Servant.API.WebSocket (WebSocket(..), )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type WebSocketApi = "health" :> WebSocket

type API
  = "users" :> Get '[JSON] [User]
  :<|> WebSocketApi

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users :<|> wsServer

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

wsServer :: Server WebSocketApi
wsServer = streamData
 where
  streamData :: MonadIO m => Connection -> m ()
  streamData c = do
    liftIO $ forkPingThread c 10
    liftIO . forM_ [1..] $ \i -> do
       sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000

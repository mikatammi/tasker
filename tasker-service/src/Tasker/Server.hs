module Tasker.Server
    ( serverMain
    ) where

import Tasker.Api (UserApi)
import qualified Tasker.Api.Types as T (User(User))

import Data.Proxy
import Data.Aeson (ToJSON)

import Servant (Server, Application, serve)
import Network.Wai.Handler.Warp (run)

instance ToJSON T.User

server1 :: Server UserApi
server1 = return [T.User "HelloWorld" "hello@world.test"]

userApi :: Proxy UserApi
userApi = Proxy

app1 :: Application
app1 = serve userApi server1

serverMain :: IO ()
serverMain = do
  putStrLn "serverMain"
  -- TODO: Get port from program arguments for example
  run 8081 app1

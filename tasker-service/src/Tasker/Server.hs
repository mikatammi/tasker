{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tasker.Server
    ( serverMain
    ) where

import Tasker.Api (UnprotectedApi, UserApi)
import qualified Tasker.Api.Types as T (Login(Login), User(User))

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(Proxy))
import Data.Aeson (FromJSON, ToJSON)

import Servant
  ( (:<|>)((:<|>))
  , (:>)
  , Application
  , Context((:.), EmptyContext)
  , Handler
  , Header
  , Headers
  , NoContent(NoContent)
  , Server
  , err401
  , serveWithContext
  , throwError
  )
import Servant.Auth.Server
  ( Auth
  , AuthResult(Authenticated)
  , CookieSettings
  , FromJWT
  , JWT
  , JWTSettings
  , SetCookie
  , ToJWT
  , acceptLogin
  , defaultCookieSettings
  , defaultJWTSettings
  , generateKey
  )

import Network.Wai.Handler.Warp (run)

instance FromJSON T.Login
instance FromJSON T.User
instance FromJWT T.User
instance ToJSON T.User
instance ToJWT T.User

checkCreds :: CookieSettings
           -> JWTSettings
           -> T.Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                               NoContent)
checkCreds cs jwts (T.Login username password) = do
  liftIO $ putStrLn $ "Username: " <> username <> " Password: " <> password
  -- TODO: Add proper login handling
  if username == "keijo" && password == "kojootti"
    then do
      let usr = T.User "Keijo Kojootti" "keijo@kaijo.kelju"
      mApplyCookies <- liftIO $ acceptLogin cs jwts usr
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent
    else
      throwError err401

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedApi
unprotectedServer cs jwts = checkCreds cs jwts

userApiServer :: AuthResult T.User -> Server UserApi
userApiServer (Authenticated user) = return [user]
userApiServer _ = throwError err401

type Api auths = (Auth auths T.User :> UserApi) :<|> UnprotectedApi

server :: CookieSettings -> JWTSettings -> Server (Api auths)
server cs jwts = userApiServer :<|> unprotectedServer cs jwts

serverMain :: IO ()
serverMain = do
  putStrLn "serverMain"
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (Api '[JWT])
      app = serveWithContext api cfg (server defaultCookieSettings jwtCfg)

  -- TODO: Get port from program arguments for example
  run 8081 $ app

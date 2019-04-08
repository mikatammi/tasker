{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tasker.Server
    ( serverMain
    ) where

import Tasker.Api (UnprotectedApi, UserApi)
import qualified Tasker.Api.Types as T (Login(Login), User(User))
import qualified Tasker.Server.Types.PostgresSettings as ST (
  PostgresSettings( PostgresSettings
                  , dbHost
                  , dbPort
                  , dbUser
                  , dbPass
                  , dbDb))

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(Proxy))
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)

import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement

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

sumStatement :: Statement.Statement (Int64, Int64) Int64
sumStatement = Statement.Statement sql encoder decoder True where
  sql = "select $1 + $2"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow (Decoders.column Decoders.int8)

sumSession :: Int64 -> Int64 -> Session.Session Int64
sumSession a b = Session.statement (a, b) sumStatement

connectionSettingsFromPostgresSettings :: ST.PostgresSettings ->
                                          Connection.Settings
connectionSettingsFromPostgresSettings ps =
  Connection.settings h po u pa db
  where
    h = (ST.dbHost ps)
    po = (ST.dbPort ps)
    u = (ST.dbUser ps)
    pa = (ST.dbPass ps)
    db = (ST.dbDb ps)

serverMain :: IO ()
serverMain = do
  putStrLn "serverMain"
  myKey <- generateKey
  let ps = ST.PostgresSettings "localhost"
                               5432
                               "tasker"
                               "01078ce73029a896f5e67c483f1d8ff936ac8724713123ed90bf04aee71125bf"
                               "tasker"
      connectionSettings = connectionSettingsFromPostgresSettings ps
  maybeConnection <- Connection.acquire connectionSettings
  case maybeConnection of Left err -> case err of Just e -> error $ "Connection error: " <> show e
                                                  Nothing -> error "Connection error"
                          Right connection -> do
                            result <- Session.run (sumSession 1 2) connection
                            putStrLn "Hello from Hasql Postgresql connection"
                            case result of Left err -> error $ show err
                                           Right r -> putStrLn $ show r
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (Api '[JWT])
      app = serveWithContext api cfg (server defaultCookieSettings jwtCfg)

  -- TODO: Get port from program arguments for example
  run 8081 $ app

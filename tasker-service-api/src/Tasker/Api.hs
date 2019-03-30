{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tasker.Api
  ( UnprotectedApi
  , UserApi
  ) where

import GHC.Generics
import Servant.API
  ( (:>)
  , Get
  , Header
  , Headers
  , JSON
  , NoContent
  , PostNoContent
  , ReqBody
  )
import Servant.Auth.Server (SetCookie)
import qualified Tasker.Api.Types as T

type UnprotectedApi =
  "login"
    :> ReqBody '[JSON] T.Login
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie]
                                      NoContent)

type UserApi = "users" :> Get '[JSON] [T.User]

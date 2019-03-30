{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tasker.Api
  ( UserApi
  ) where

import GHC.Generics
import Servant.API ((:>), Get, JSON)
import qualified Tasker.Api.Types as T

type UserApi = "users" :> Get '[JSON] [T.User]

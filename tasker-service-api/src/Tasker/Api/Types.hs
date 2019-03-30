{-# LANGUAGE DeriveGeneric #-}

module Tasker.Api.Types
  ( Login(Login, username, password)
  , User(User)
  ) where

import GHC.Generics

data Login = Login
  { username :: String
  , password :: String
  } deriving (Eq, Show, Generic)

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

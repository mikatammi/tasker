{-# LANGUAGE DeriveGeneric #-}

module Tasker.Api.Types
  ( User(User)
  ) where

import GHC.Generics

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

module Tasker.Server.Types.PostgresSettings
    ( PostgresSettings(PostgresSettings, dbHost, dbPort, dbUser, dbPass, dbDb)
    ) where

import Data.ByteString (ByteString)
import GHC.Word (Word16)

data PostgresSettings = PostgresSettings
  { dbHost :: ByteString
  , dbPort :: Word16
  , dbUser :: ByteString
  , dbPass :: ByteString
  , dbDb   :: ByteString
  } deriving (Eq, Show)

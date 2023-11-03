module Db (withConn) where

import Database.PostgreSQL.Simple (ConnectInfo (ConnectInfo), Connection, withConnect)

withConn :: (Connection -> IO a) -> IO a
withConn = withConnect (ConnectInfo "localhost" 5432 "admin" "admin" "badminton")
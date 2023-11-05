{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ingestion.Players.Store (createPlayers, getPlayers) where

import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Ingestion.Players.Types (Player (..))

createPlayers :: [Player] -> IO ()
createPlayers clubs = do
  s <- withConn (`insertPlayers` clubs)
  print $ "Rows affected: " <> show s

insertPlayers :: Connection -> [Player] -> IO Int64
insertPlayers conn clubs = do
  executeMany
    conn
    [sql|
      insert into players ("Name", "Url") values (?,?) 
      on conflict ("Name") do nothing;
    |]
    ( Prelude.map
        (\c -> (playerName c, playerUrl c))
        clubs
    )

getPlayers :: IO [Player]
getPlayers = withConn (`query_` [sql|select \"Name\", \"Url\" from players|])

instance FromRow Player where
  fromRow = Player <$> field <*> field
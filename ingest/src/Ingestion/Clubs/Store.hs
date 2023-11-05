{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ingestion.Clubs.Store (createClubs, getClubs) where

import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Ingestion.Clubs.Types

createClubs :: [Club] -> IO ()
createClubs clubs = do
  s <- withConn (`insertClubs` clubs)
  print $ "Rows affected: " <> show s

insertClubs :: Connection -> [Club] -> IO Int64
insertClubs conn clubs = do
  executeMany
    conn
    [sql|
      insert into clubs ("FullName", "Url", "ShortName") values (?,?,?) 
      on conflict ("FullName") do nothing;
    |]
    ( Prelude.map
        (\c -> (clubFullName c, clubUrl c, clubShortName c))
        clubs
    )

getClubs :: IO [Club]
getClubs = withConn (`query_` [sql| select "FullName", "Url", "ShortName" from clubs |])

instance FromRow Club where
  fromRow = Club <$> field <*> field <*> field
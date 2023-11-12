{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ingestion.Clubs.Store (createClubs, getClubs, createTeamsForClub) where

import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Ingestion.Clubs.Types
import Ingestion.Divisions.Types

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
getClubs = withConn (`query_` [sql| select "Id", "FullName", "Url", "ShortName" from clubs where "ShortName" != 'N/A' |])

instance FromRow Club where
  fromRow = Club <$> field <*> field <*> field <*> field

createTeamsForClub :: Club -> [Team] -> IO ()
createTeamsForClub club teams = do
  s <- withConn (insertTeams club teams)
  print $ "Rows affected: " <> show s

insertTeams :: Club -> [Team] -> Connection -> IO Int64
insertTeams club teams conn = do
  executeMany
    conn
    [sql| 
      update teams
      set ClubId = ?
      where "Name" = ?
    |]
    (map (\t -> (clubId club, teamName t)) teams)
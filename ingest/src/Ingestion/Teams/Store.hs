{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ingestion.Teams.Store (createTeams) where

import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Ingestion.Teams.Types (Team (..))

createTeams :: [Team] -> IO ()
createTeams teams = do
  s <- withConn (`insertTeams` teams)
  print $ "Rows affected: " <> show s

insertTeams :: Connection -> [Team] -> IO Int64
insertTeams conn teams = do
  executeMany
    conn
    [sql|
      insert into teams ("Name", "Url") values (?,?,?,?) 
      on conflict ("LeagueName") do nothing;
    |]
    ( Prelude.map
        (\t -> (teamName t, teamUrl t))
        teams
    )
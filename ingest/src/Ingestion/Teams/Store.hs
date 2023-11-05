{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ingestion.Teams.Store (createTeams) where

import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Ingestion.Teams.Types (Team (..))

createTeams :: [Team] -> IO ()
createTeams clubs = do
  s <- withConn (`insertTeams` clubs)
  print $ "Rows affected: " <> show s

insertTeams :: Connection -> [Team] -> IO Int64
insertTeams conn clubs = do
  executeMany
    conn
    [sql|
      insert into Team ("Name", "Url") values (?,?) 
      on conflict ("Name") do nothing;
    |]
    ( Prelude.map
        (\c -> (teamName c, teamUrl c))
        clubs
    )
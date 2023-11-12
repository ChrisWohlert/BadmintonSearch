{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use mapMaybe" #-}

module Ingestion.Divisions.Store (createDivisions) where

import Control.Monad
import Data.Int
import Data.Map hiding (map)
import Data.Maybe (catMaybes)
import Data.Text hiding (concat, concatMap, map)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Ingestion.Divisions.Types (Division (..), DivisionGroup (divisionGroupName, divisionGroupTeams, divisionGroupUrl), Team (teamName))
import Prelude hiding (lookup)

createDivisions :: [Division] -> IO ()
createDivisions divisions = do
  print divisions
  s <- withConn (`insertDivisions` divisions)
  print $ "Rows affected: " <> show s

insertDivisions :: Connection -> [Division] -> IO Int64
insertDivisions conn divisions = do
  divisionMap <- insertAndReturnDivisions
  print $ keys divisionMap
  groupMap <- insertAndReturnGroups divisionMap
  insertTeams groupMap
  where
    insertAndReturnDivisions :: IO (Map Text Int)
    insertAndReturnDivisions =
      executeMany
        conn
        [sql|

            insert into divisions ("Name") values (?) 
            on conflict ("Name") do nothing
          |]
        (map (Only . divisionName) divisions)
        >> fromList <$> query_ conn [sql| select "Name", "Id" from divisions |]
    insertAndReturnGroups :: Map Text Int -> IO (Map Text Int)
    insertAndReturnGroups divisionMap =
      executeMany
        conn
        [sql|

            insert into groups ("Name", "Url", "DivisionId") values (?,?,?) 
            on conflict ("Name", "DivisionId") do nothing
          |]
        ( concat $ catMaybes $ map (divisionToGroupRow divisionMap) divisions
        )
        >> fromList <$> query_ conn [sql| select "Name", "Id" from groups |]
    divisionToGroupRow :: Map Text Int -> Division -> Maybe [(Text, Text, Int)]
    divisionToGroupRow divisionMap d = do
      divisionId <- lookup (divisionName d) divisionMap
      pure $ map (\g -> (divisionGroupName g, divisionGroupUrl g, divisionId)) (divisionGroups d)

    insertTeams :: Map Text Int -> IO Int64
    insertTeams groupMap =
      let teams = concatMap (map (groupToTeamRow groupMap) . divisionGroups) divisions
       in executeMany
            conn
            [sql|

                insert into teams ("Name", "GroupId") values (?,?) 
                on conflict ("Name", "GroupId") do nothing
              |]
            (concat $ catMaybes teams)

    groupToTeamRow :: Map Text Int -> DivisionGroup -> Maybe [(Text, Int)]
    groupToTeamRow groupMap divisionGroup = do
      groupId <- lookup (divisionGroupName divisionGroup) groupMap
      pure $ map (\t -> (teamName t, groupId)) (divisionGroupTeams divisionGroup)

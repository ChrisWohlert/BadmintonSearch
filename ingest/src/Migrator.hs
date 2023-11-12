{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Migrator (migrate, setupMigration) where

import Control.Monad
import Data.Foldable
import Data.List ((\\))
import Data.String
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import System.Directory

migrationFolder :: String
migrationFolder = "Migrations"

migrate :: IO ()
migrate = do
  files <- filter (\x -> length x > 2) <$> getDirectoryContents migrationFolder
  print files
  filesAlreadyMigrated <- withConn (`query_` "select \"Filename\" from migrations")
  traverse_ migrateFile (files \\ filesAlreadyMigrated)
  where
    migrateFile :: FilePath -> IO ()
    migrateFile file = do
      print $ "Migrating file: " <> migrationFolder <> "/" <> file
      sqlInFile <- readFile $ migrationFolder <> "/" <> file
      now <- getCurrentTime
      void $
        withConn
          ( \conn -> do
              void $ execute_ conn $ fromString sqlInFile
              execute
                conn
                [sql|
                  insert into migrations ("Filename", "CreatedTime") values (?,?) 
                |]
                (file, now)
          )

setupMigration :: IO ()
setupMigration = do
  sqlToRun <- readFile "setup-migration.sql"
  void $ withConn (`execute_` fromString sqlToRun)

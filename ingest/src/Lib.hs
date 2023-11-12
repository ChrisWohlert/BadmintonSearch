module Lib
  ( someFunc,
  )
where

import Data.Map
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding
import Ingestion.Clubs.Service (ingestClubs, ingestTeamsToClubs)
import Ingestion.Divisions.Service
import Ingestion.Players.Service (ingestPlayers)
import Migrator
import Text.Read (readMaybe)
import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = do
  setLocaleEncoding utf8
  runApp

-- runApp

runApp :: IO ()
runApp = do
  let actions =
        fromList $
          zip
            [1 ..]
            [ ("Ingest clubs", ingestClubs >> runApp),
              ("Ingest players", ingestPlayers >> runApp),
              ("Ingest divisions", ingestDivisions >> runApp),
              ("Ingest teams for clubs", ingestTeamsToClubs >> runApp),
              ("Migrate db", migrate >> runApp),
              ("Setup migrations db", setupMigration >> runApp)
            ]
  putStrLn $ unlines $ foldrWithKey (\k (t, _) b -> show (k :: Int) <> ") " <> t : b) [] actions
  putStrLn "Anything else to quit"
  input <- getLine
  fromMaybe (pure ()) (handleInput actions input)

handleInput :: Map Int (String, IO ()) -> String -> Maybe (IO ())
handleInput actions input = do
  i <- readMaybe input
  (_, action) <- lookup i actions
  pure action

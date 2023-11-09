module Lib
  ( someFunc,
  )
where

import GHC.IO.Encoding
import Ingestion.Clubs.Service (ingestClubs)
import Ingestion.Players.Service (ingestPlayers)
import Ingestion.Teams.Service

someFunc :: IO ()
someFunc = do
  setLocaleEncoding utf8
  runApp

runApp :: IO ()
runApp = do
  putStrLn "1) Ingest clubs"
  putStrLn "2) Ingest players"
  putStrLn "3) Ingest teams"
  putStrLn "Anything else to quit"
  input <- getLine
  case input of
    "1" -> ingestClubs >> runApp
    "2" -> ingestPlayers >> runApp
    "3" -> ingestTeams >> runApp
    _ -> return ()
module Lib
  ( someFunc,
  )
where

import GHC.IO.Encoding
import Ingestion.Clubs.Service (ingestClubs)
import Ingestion.Players.Service (ingestPlayers)

someFunc :: IO ()
someFunc = do
  setLocaleEncoding utf8
  runApp

runApp :: IO ()
runApp = do
  putStrLn "1) Ingest clubs"
  putStrLn "2) Ingest players"
  putStrLn "Anything else to quit"
  input <- getLine
  case input of
    "1" -> ingestClubs >> runApp
    "2" -> ingestPlayers >> runApp
    _ -> return ()
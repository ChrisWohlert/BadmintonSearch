module Lib
  ( someFunc,
  )
where

import GHC.IO.Encoding
import Ingestion.Club (ingestClubs)

someFunc :: IO ()
someFunc = do
  setLocaleEncoding utf8
  runApp

runApp :: IO ()
runApp = do
  putStrLn "1) Ingest clubs"
  putStrLn "Anything else to quit"
  input <- getLine
  case input of
    "1" -> do
      ingestClubs
      runApp
    _ -> return ()
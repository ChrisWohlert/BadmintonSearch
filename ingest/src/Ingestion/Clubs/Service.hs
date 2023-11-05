module Ingestion.Clubs.Service (ingestClubs) where

import Ingestion.Clubs.Ingest (scrapeClubs)
import Ingestion.Clubs.Store (createClubs)

ingestClubs :: IO ()
ingestClubs = scrapeClubs createClubs
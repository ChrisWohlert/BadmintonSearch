module Ingestion.Divisions.Service (ingestDivisions) where

import Ingestion.Divisions.Ingest (scrapeDivisions)
import Ingestion.Divisions.Store (createDivisions)

ingestDivisions :: IO ()
ingestDivisions = do
  divisions <- scrapeDivisions
  createDivisions divisions
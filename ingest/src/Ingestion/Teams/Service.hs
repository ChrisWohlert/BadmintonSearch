module Ingestion.Teams.Service (ingestTeams) where

import Ingestion.Clubs.Store (getClubs)
import Ingestion.Clubs.Types
import Ingestion.Teams.Ingest (scrapeTeams)
import Ingestion.Teams.Store (createTeams)

ingestTeams :: IO ()
ingestTeams = do
  clubNames <- take 2 . map clubShortName <$> getClubs
  teams <- scrapeTeams clubNames
  createTeams teams
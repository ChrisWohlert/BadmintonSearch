module Ingestion.Teams.Service (ingestPlayers) where

import Ingestion.Clubs.Store (getClubs)
import Ingestion.Clubs.Types
import Ingestion.Teams.Ingest (scrapeTeams)
import Ingestion.Teams.Store (createTeams)

ingestPlayers :: IO ()
ingestPlayers = do
  clubNames <- map clubShortName <$> getClubs
  teams <- scrapeTeams clubNames
  createTeams teams
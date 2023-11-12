module Ingestion.Clubs.Service (ingestClubs, ingestTeamsToClubs) where

import Data.Foldable
import Ingestion.Clubs.Ingest (scrapeClubs)
import Ingestion.Clubs.Store (createClubs, createTeamsForClub, getClubs)
import Ingestion.Divisions.Ingest (scrapeTeamsForClub)

ingestClubs :: IO ()
ingestClubs = scrapeClubs createClubs

ingestTeamsToClubs :: IO ()
ingestTeamsToClubs = do
  clubs <- getClubs
  clubTeamsPairs <- scrapeTeamsForClub clubs
  traverse_ (uncurry createTeamsForClub) clubTeamsPairs

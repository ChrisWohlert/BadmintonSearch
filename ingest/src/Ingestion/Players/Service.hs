module Ingestion.Players.Service (ingestPlayers) where

import Ingestion.Players.Ingest (scrapePlayers)
import Ingestion.Players.Store (createPlayers)

ingestPlayers :: IO ()
ingestPlayers = scrapePlayers >>= createPlayers
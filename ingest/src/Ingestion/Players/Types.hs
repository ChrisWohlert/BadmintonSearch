module Ingestion.Players.Types (Player (..)) where

import Data.Text

data Player = Player {playerName :: Text, playerUrl :: Text}
  deriving (Show)
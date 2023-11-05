module Ingestion.Teams.Types (Team (..)) where

import Data.Text

data Team = Team {teamName :: Text, teamUrl :: Text}
  deriving (Show)
module Ingestion.Teams.Types (Team (..), Division(..), DivisionGroup(..)) where

import Data.Text

data Team = Team {teamName :: Text, teamUrl :: Text}
  deriving (Show)


data Division = Division { divisionName :: Text, divisionGroups :: [DivisionGroup] }

data DivisionGroup = DivisionGroup { divisionGroupName :: Text, divisionGroupTeams :: [Team] }
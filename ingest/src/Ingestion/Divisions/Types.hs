module Ingestion.Divisions.Types (Team (..), Division (..), DivisionGroup (..)) where

import Data.Text

newtype Team = Team {teamName :: Text}
  deriving (Show)

data Division = Division {divisionName :: Text, divisionGroups :: [DivisionGroup]}
  deriving (Show)

data DivisionGroup = DivisionGroup {divisionGroupName :: Text, divisionGroupUrl :: Text, divisionGroupTeams :: [Team]}
  deriving (Show)

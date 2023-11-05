module Ingestion.Clubs.Types (Club (..)) where

import Data.Text

data Club = Club {clubFullName :: Text, clubUrl :: Text, clubShortName :: Text}
  deriving (Show)
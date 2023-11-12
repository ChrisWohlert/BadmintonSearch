module Ingestion.Clubs.Types (Club (..)) where

import Data.Text

data Club = Club {clubId :: Int, clubFullName :: Text, clubUrl :: Text, clubShortName :: Text}
  deriving (Show)
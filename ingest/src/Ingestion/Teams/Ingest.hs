{-# LANGUAGE OverloadedStrings #-}

module Ingestion.Teams.Ingest (scrapeTeams) where

import Control.Monad
import Data.Either (fromRight)
import Data.Text hiding (spanM)
import Ingestion.Teams.Types (Team (..))
import Scraper
import Util
import Web.Api.WebDriver
import Web.Api.WebDriver.Endpoints ()
import WebDriverHelper

scrapeTeams :: [Text] -> IO [Team]
scrapeTeams names =
  runDriver
    ( do
        void maximizeWindow
        runScraper names
    )

runScraper :: [Text] -> WebDriver [Team]
runScraper (name : names) = do
  navigateTo "https://badmintonplayer.dk/DBF/HoldTurnering/Stilling/"
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  _ <- findElem "#ctl00_ContentPlaceHolder1_ShowStandings1_SelectClubDropDown1_TextBoxName" >>= elementSendKeys name
  _ <- findElem "#ctl00_ContentPlaceHolder1_ShowStandings1_ButtonSearchClub" >>= click
  rows <- findElems ".clubgrouplist * > tr"

isLink :: ElementRef -> WebDriver Bool
isLink e = do
  url <- getElementAttribute "href" e
  return $ case url of
    Right "#" -> True
    _ -> False

mapClubLineToClub :: ElementRef -> WebDriver Team
mapClubLineToClub c = do
  t <- getText c
  url <- getElementAttribute "href" c
  return $ Team t (fromRight "url" url)

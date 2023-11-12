{-# LANGUAGE OverloadedStrings #-}

module Ingestion.Divisions.Ingest (scrapeDivisions, scrapeTeamsForClub) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (fromRight)
import Data.Functor
import Ingestion.Clubs.Types (Club (clubShortName))
import Ingestion.Divisions.Types (Division (..), DivisionGroup (..), Team (..))
import Scraper
import Util (flattenOn)
import Web.Api.WebDriver
import qualified Web.Api.WebDriver as Keys
import Web.Api.WebDriver.Endpoints ()
import WebDriverHelper

scrapeDivisions :: IO [Division]
scrapeDivisions =
  runDriver
    ( do
        void maximizeWindow
        runDivisionScraper
    )

runDivisionScraper :: WebDriver [Division]
runDivisionScraper = do
  navigateTo "https://badmintonplayer.dk/DBF/HoldTurnering/Stilling/"
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  -- findElem "#ctl00_ContentPlaceHolder1_ShowStandings1_DropDownListRegion" >>= (\x -> click x >> elementSendKeys (pack [keyToChar Keys.ArrowDownKey, keyToChar Keys.EnterKey]) x)
  "#ctl00_ContentPlaceHolder1_ShowStandings1_ButtonSearch" >>= click
  rows <- ".selectgroup * > tr"
  parseDivisions rows
  where
    parseDivisions :: [ElementRef] -> WebDriver [Division]
    parseDivisions [] = pure []
    parseDivisions es = do
      rowGroups <-
        map (map snd) . flattenOn fst
          <$> traverse
            ( \x -> do
                isDivision <- getElementAttribute "class" x <&> (("divisionrow" ==) . fromRight "")
                return (isDivision, x)
            )
            es
      names <- traverse (traverse getText) rowGroups
      printIO names
      mapToDivisions rowGroups

printIO :: (Show a) => a -> WebDriver ()
printIO = liftIO . print

mapToDivisions :: [[ElementRef]] -> WebDriver [Division]
mapToDivisions [] = pure []
mapToDivisions ([] : es) = mapToDivisions es
mapToDivisions ((division : leagues) : es) = do
  names <- (,) <$> getText division <*> traverse getText leagues
  liftIO $ print names
  dName <- division |-> "h3" >>= getText
  groups <-
    traverse
      ( \league -> do
          link <- league |-> "a"
          Right onClick <- getElementAttribute "onclick" link
          lName <- getText link
          divisionUrl <- getCurrentUrl
          divisions <- do
            runInNewWindow $ do
              navigateTo "https://badmintonplayer.dk/DBF/HoldTurnering/Stilling/#1,2023,,1,1,,,,"
              _ <- executeScript onClick []
              rows <- drop 1 <$> ".groupstandings * > tr"
              traverse
                ( \r -> do
                    tName <- r |-> "a" >>= getText
                    return $ Team tName
                )
                rows
          return $ DivisionGroup lName divisionUrl divisions
      )
      leagues
  (Division dName groups :) <$> mapToDivisions es

scrapeTeamsForClub :: [Club] -> IO [(Club, [Team])]
scrapeTeamsForClub clubs =
  runDriver
    ( do
        void maximizeWindow
        runTeamsScraper clubs
    )

runTeamsScraper :: [Club] -> WebDriver [(Club, [Team])]
runTeamsScraper clubs = do
  navigateTo "https://badmintonplayer.dk/DBF/HoldTurnering/Stilling/"
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  traverse runTeamsScraperForClub clubs
  where
    runTeamsScraperForClub :: Club -> WebDriver (Club, [Team])
    runTeamsScraperForClub club = do
      navigateTo "https://badmintonplayer.dk/DBF/HoldTurnering/Stilling/"
      "#ctl00_ContentPlaceHolder1_ShowStandings1_SelectClubDropDown1_TextBoxName" >>= \searchBox ->
        sendKeys (" " <> clubShortName club <> keysToText [Keys.HomeKey, Keys.DeleteKey, Keys.EnterKey]) searchBox
      teams <- ".grouprow > td:nth-child(1)" >>= traverse getText
      return (club, map Team teams)

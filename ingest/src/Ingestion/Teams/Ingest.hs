{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Ingestion.Teams.Ingest (scrapeTeams) where

import Control.Monad
import Data.Either (fromRight)
import Data.Text hiding (drop, concatMap, map, concat, spanM)
import Ingestion.Teams.Types (Team (..), Division(..), DivisionGroup (..))
import Scraper
import Util
import Web.Api.WebDriver
import Web.Api.WebDriver.Endpoints ()
import WebDriverHelper
import Data.List.Split (splitWhen)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Web.Api.WebDriver.Types.Keyboard as Keys
import Data.Functor

scrapeTeams :: [Text] -> IO [Team]
scrapeTeams names =
  runDriver
    ( do
        void maximizeWindow
        runScraper <&> (concatMap divisionGroupTeams . concatMap divisionGroups)
    )

runScraper :: WebDriver [Division]
runScraper = scrapeDivisions

scrapeDivisions :: WebDriver [Division]
scrapeDivisions = do
  navigateTo "https://badmintonplayer.dk/DBF/HoldTurnering/Stilling/"
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  findElem "#ctl00_ContentPlaceHolder1_ShowStandings1_DropDownListRegion" >>= (\ x -> click x >> elementSendKeys (pack [keyToChar Keys.ArrowDownKey, keyToChar Keys.EnterKey]) x)
  wait 10000
  "#ctl00_ContentPlaceHolder1_ShowStandings1_ButtonSearch" >>= click
  wait 10000
  rows <- findElems ".selectgroup * > tr"
  parseTeams rows
  where
    parseTeams :: [ElementRef] -> WebDriver [Division]
    parseTeams [] = pure []
    parseTeams es = do
      rowGroups <- map (map snd) . splitWhen fst <$> traverse (\ x -> do
        isDivision <- getElementAttribute "class" x <&> (("divisionrow" ==) . fromRight "")
        return (isDivision, x)
        ) es
      mapToDivisions rowGroups

mapToDivisions :: [[ElementRef]] -> WebDriver [Division]
mapToDivisions [] = pure []
mapToDivisions ([]:es) = mapToDivisions es
mapToDivisions ((division:leagues):es) = do
  dName <- division |-> "h3" >>= getText
  groups <- traverse (\ league -> do
    link <- league |-> "a"
    lName <- getText link
    teams <- do
      click link
      rows <- drop 1 <$> findElems ".groupstandings * > tr"
      ts <- traverse (\ r -> do
        tName <- r |-> "a" >>= getText
        return $ Team tName "") rows
      goBack
      return ts
    return $ DivisionGroup lName teams) leagues
  (Division dName groups :) <$> mapToDivisions es


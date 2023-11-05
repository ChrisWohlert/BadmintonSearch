{-# LANGUAGE OverloadedStrings #-}

module Ingestion.Players.Ingest (scrapePlayers) where

import Control.Monad
import Data.Either (fromRight)
import Ingestion.Players.Types (Player (..))
import Scraper
import Util
import Web.Api.WebDriver (ElementRef)
import Web.Api.WebDriver.Endpoints
import WebDriverHelper

scrapePlayers :: IO [Player]
scrapePlayers =
  runDriver
    ( do
        void maximizeWindow
        runScraper
    )

runScraper :: WebDriver [Player]
runScraper = do
  navigateTo "https://badmintonplayer.dk/DBF/Ranglister/#287,2023,,0,,,,0,,,,,,,,0,,,,,,"
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  getPlayersForPage
  where
    getPlayersForPage :: WebDriver [Player]
    getPlayersForPage = do
      tableLines <- waitUntil 10 ".RankingListGrid * > a"
      (links, clubLines) <- spanM isLink tableLines
      clubs <- traverse mapClubLineToClub clubLines
      nextPageLink <- elemWithText links ">"
      case nextPageLink of
        Just link -> do
          click link
          moreClubs <- getPlayersForPage
          return $ clubs ++ moreClubs
        _ -> return clubs

isLink :: ElementRef -> WebDriver Bool
isLink e = do
  url <- getElementAttribute "href" e
  return $ case url of
    Right "#" -> True
    _ -> False

mapClubLineToClub :: ElementRef -> WebDriver Player
mapClubLineToClub c = do
  t <- getText c
  url <- getElementAttribute "href" c
  return $ Player t (fromRight "url" url)

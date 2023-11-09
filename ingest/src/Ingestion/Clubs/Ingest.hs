{-# LANGUAGE OverloadedStrings #-}

module Ingestion.Clubs.Ingest (scrapeClubs) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List.Split
import Data.Text hiding (chunksOf, spanM)
import Ingestion.Clubs.Types
import Scraper
import Util
import Web.Api.WebDriver (ElementRef)
import Web.Api.WebDriver.Endpoints
import Web.Api.WebDriver.Monad
import WebDriverHelper

scrapeClubs :: ([Club] -> IO ()) -> IO ()
scrapeClubs create =
  runDriver
    ( do
        void maximizeWindow
        runScraper create
    )

baseUrl :: Url
baseUrl = "https://badmintonplayer.dk"

runScraper :: ([Club] -> IO ()) -> WebDriver ()
runScraper create = do
  navigateTo "https://badmintonplayer.dk/DBF/se-hvor-du-kan-spille-badminton/#1,,,,,0,,"
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  clubNamesAndUrls <- getClubsForPage 1
  let batches = chunksOf 20 clubNamesAndUrls
  liftIO $ print $ Prelude.length batches
  traverse_ (traverse getClubFromUrl >=> liftIO . create) batches
  where
    getClubsForPage :: Int -> WebDriver [(Text, Text)]
    getClubsForPage page = do
      tableLines <- "#ctl00_ContentPlaceHolder1_ShowClub1_PanelSearchResult * > a"
      (links, clubLines) <- spanM isLink tableLines
      clubs <- traverse mapClubLineToClub clubLines
      if page < Prelude.length links
        then do
          let link = links !! page
          click link
          wait 200000
          moreClubs <- getClubsForPage (page + 1)
          return $ clubs ++ moreClubs
        else return clubs

getClubFromUrl :: (Text, Text) -> WebDriver Club
getClubFromUrl (name, url) = do
  navigateTo $ baseUrl <> url
  shortName <- ("#ctl00_ContentPlaceHolder1_ClubInfo1_LabelEditName" >>= getText) `onTimeout` pure "N/A"
  return $ Club name url shortName

isLink :: ElementRef -> WebDriver Bool
isLink e = do
  url <- getElementAttribute "href" e
  return $ case url of
    Right "#" -> True
    _ -> False

mapClubLineToClub :: ElementRef -> WebDriver (Text, Text)
mapClubLineToClub c = do
  t <- getText c
  url <- getElementAttribute "href" c
  return (t, fromRight "url" url)
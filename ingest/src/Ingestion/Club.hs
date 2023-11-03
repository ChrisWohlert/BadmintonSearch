{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ingestion.Club (ingestClubs) where

import Control.Monad
import Data.Bifunctor
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Int
import Data.Text hiding (spanM)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Db
import Scraper
import Web.Api.WebDriver (ElementRef)
import Web.Api.WebDriver.Endpoints
import Web.Api.WebDriver.Monad
import WebDriverHelper

data Club = Club {clubName :: Text, clubUrl :: Text}
  deriving (Show)

ingestClubs :: IO ()
ingestClubs = do
  clubs <-
    runDriver
      ( do
          void maximizeWindow
          scrapeClubs
      )
  s <- withConn (`insertClubs` clubs)
  print $ "Rows affected: " <> show s

insertClubs :: Connection -> [Club] -> IO Int64
insertClubs conn clubs = do
  executeMany
    conn
    [sql|
      insert into clubs ("Name", "Url") values (?,?) 
      on conflict ("Name") do nothing;
    |]
    ( Prelude.map
        (\c -> (clubName c, clubUrl c))
        clubs
    )

scrapeClubs :: WebDriver [Club]
scrapeClubs = do
  navigateTo "https://badmintonplayer.dk/DBF/se-hvor-du-kan-spille-badminton/#1,,,,,0,,"
  wait 200000
  waitUntil 10 $ "#didomi-notice-disagree-button" >>= click
  getClubsForPage 1
  where
    getClubsForPage :: Int -> WebDriver [Club]
    getClubsForPage page = do
      tableLines <- findElems "#ctl00_ContentPlaceHolder1_ShowClub1_PanelSearchResult * > a"
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

isLink :: ElementRef -> WebDriver Bool
isLink e = do
  url <- getElementAttribute "href" e
  return $ case url of
    Right "#" -> True
    _ -> False

mapClubLineToClub :: ElementRef -> WebDriver Club
mapClubLineToClub c = do
  t <- getText c
  url <- getElementAttribute "href" c
  return $ Club t (fromRight "url" url)

spanM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = pure ([], [])
spanM f (x : xs) = do
  p <- f x
  bool second first p (x :) <$> spanM f xs
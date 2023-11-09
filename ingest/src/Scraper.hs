{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper
  ( elemWithText,
    (|>=),
    (|->),
    (|-->),
    (|><),
    WebDriver,
    click,
    getText,
    findElem,
    findElems,
    findElemFrom,
    findElemsFrom,
    waitUntil,
    onTimeout,
  )
where

import Data.String hiding (words)
import Data.Text hiding (drop, filter, length, map, take, zip)
import Debug.Trace
import Web.Api.WebDriver

type WebDriver m = WebDriverT IO m

type Element = ElementRef

type WebElement = WebDriver Element

type WebElements = WebDriver [Element]

instance IsString WebElement where
  fromString :: String -> WebDriver Element
  fromString = findElement CssSelector . pack

instance IsString WebElements where
  fromString :: String -> WebDriver [Element]
  fromString = findElements CssSelector . pack

elemWithText :: [Element] -> Text -> WebDriver (Maybe Element)
elemWithText [] _ = pure Nothing
elemWithText (x : xs) t = do
  text <- getText x
  if text == t then pure $ Just x else elemWithText xs t

(|>=) :: WebDriver Element -> (Element -> WebDriver b) -> WebDriver b
selector |>= f = waitUntil 15 selector >>= f

(|->) :: Element -> Selector -> WebDriver Element
element |-> selector = findElemFrom element selector

(|-->) :: Element -> Selector -> WebDriver [Element]
element |--> selector = findElemsFrom element selector

(|><) :: WebDriver Element -> WebDriver a -> WebDriver a
e |>< f = do
  e |>= click
  y <- waitUntil 15 f
  goBack
  pageRefresh
  return y

infixr 1 |><

click :: Element -> WebDriver ()
click = elementClick

getText :: Element -> WebDriver Text
getText = getElementText

findElem :: Selector -> WebDriver Element
findElem = findElement CssSelector

findElems :: Selector -> WebDriver [Element]
findElems = findElements CssSelector

findElemFrom :: Element -> Selector -> WebDriver Element
findElemFrom e s = findElementFromElement CssSelector s e

findElemsFrom :: Element -> Selector -> WebDriver [Element]
findElemsFrom e s = findElementsFromElement CssSelector s e

waitUntil :: Int -> WebDriver a -> WebDriver a
waitUntil 0 f = do
  f
waitUntil i f = do
  catchError
    f
    (\x -> traceShowM ("Waiting for: " <> show x) >> wait 1000 >> waitUntil (i - 1) f)

f `onTimeout` g =
  catchError
    f
    (const g)

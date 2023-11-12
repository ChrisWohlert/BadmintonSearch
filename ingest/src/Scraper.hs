{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Scraper
  ( elemWithText,
    (|>=),
    (|->),
    (|-->),
    (|><),
    WebDriver,
    WebElement,
    WebElements,
    click,
    getText,
    findElemFrom,
    findElemsFrom,
    waitUntil,
    onTimeout,
    runInNewWindow,
    clickNewTab,
    keysToText,
    sendKeys,
  )
where

import Control.Monad
import Data.String hiding (words)
import Data.Text hiding (drop, filter, length, map, take, zip)
import Debug.Trace
import Web.Api.WebDriver

type WebDriver m = WebDriverT IO m

type Element = ElementRef

type WebElement = WebDriver Element

type WebElements = WebDriver [Element]

instance IsString WebElement where
  fromString :: String -> WebElement
  fromString = findElement CssSelector . pack

instance IsString WebElements where
  fromString :: String -> WebElements
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

runInNewWindow :: WebDriver a -> WebDriver a
runInNewWindow driver = do
  currentWindow <- getWindowHandle
  (contextId, _) <- newWindow WindowContext
  switchToWindow contextId
  result <- driver
  void closeWindow
  switchToWindow currentWindow
  return result

clickNewTab :: Element -> WebDriver a -> WebDriver a
clickNewTab e driver = do
  currentWindow <- getWindowHandle
  let !actions =
        [ ( emptyAction
              { _inputSourceType = Just KeyInputSource,
                _inputSourceId = Just "key",
                _actionItems =
                  [ emptyActionItem {_actionType = Just KeyDownAction, _actionValue = Just (pack "\x009")},
                    emptyActionItem {_actionType = Just KeyDownAction, _actionValue = Just (pack "\x006")}
                  ]
              }
          )
        ]
  let !upActions =
        [ ( emptyAction
              { _inputSourceType = Just KeyInputSource,
                _inputSourceId = Just "key",
                _actionItems =
                  [ emptyActionItem {_actionType = Just KeyUpAction, _actionValue = Just (pack "\x009")},
                    emptyActionItem {_actionType = Just KeyUpAction, _actionValue = Just (pack "\x006")}
                  ]
              }
          )
        ]
  performActions actions
  click e
  performActions upActions
  result <- driver
  void closeWindow
  switchToWindow currentWindow
  releaseActions
  return result

getText :: Element -> WebDriver Text
getText = getElementText

sendKeys :: Text -> Element -> WebDriver ()
sendKeys = elementSendKeys

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

onTimeout :: WebDriver a -> a -> WebDriver a
f `onTimeout` g =
  catchError
    f
    (const $ pure g)

keysToText :: [Key] -> Text
keysToText = pack . map keyToChar
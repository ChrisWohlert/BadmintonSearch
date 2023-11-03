module WebDriverHelper
  ( runDriver,
  )
where

import Control.Concurrent (threadDelay)
import Scraper
import Web.Api.WebDriver

runDriver :: WebDriver a -> IO a
runDriver driver = do
  threadDelay (1000 * 3000)
  (Right result, _, _) <-
    execWebDriverT
      conf
      (runIsolated defaultChromeCapabilities driver)
  return result

conf :: WebDriverConfig IO
conf = setToChrome defaultWebDriverConfig

setToChrome :: WebDriverConfig eff -> WebDriverConfig eff
setToChrome (WDConfig s r eva) =
  WDConfig
    s
    ( r
        { _env =
            defaultWDEnv
              { _remotePort = 4444
              }
        }
    )
    eva
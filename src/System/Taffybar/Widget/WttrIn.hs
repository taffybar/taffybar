{-# LANGUAGE OverloadedStrings #-}

-- | This is a simple weather widget that polls wttr.in to retrieve the weather,
-- instead of relying on noaa data.
--
-- Useful if NOAA data doesn't cover your needs, or if you just like wttr.in
-- better.
--
-- For more information on how to use wttr.in, see <https://wttr.in/:help>.
module System.Taffybar.Widget.WttrIn (textWttrNew) where

import Control.Exception as E (handle)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk (Widget)
import Network.HTTP.Client
  ( HttpException,
    Request (requestHeaders),
    Response (responseBody, responseStatus),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.Log.Logger (Priority (ERROR), logM)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelWithVariableDelayAndRefresh)
import Text.Regex (matchRegex, mkRegex)

-- | Creates a GTK Label widget that polls the requested wttr.in url for weather
-- information.
--
-- Not compatible with image endpoints and binary data, such as the %.png%
-- endpoints.
--
-- > -- Yields a label with the text "London: ⛅️  +72°F". Updates every 60
-- > -- seconds.
-- > textWttrNew "http://wttr.in/London?format=3" 60
textWttrNew ::
  MonadIO m =>
  -- | URL. All non-alphanumeric characters must be properly %-encoded.
  String ->
  -- | Update Interval (in seconds)
  Double ->
  m Widget
textWttrNew url interval = pollingLabelWithVariableDelayAndRefresh action True
  where action = do
          rsp <- callWttr url
          return (rsp, Nothing, interval)

-- | IO Action that calls wttr.in as per the user's request.
callWttr :: String -> IO T.Text
callWttr url =
  let unknownLocation rsp =
        -- checks for a common wttr.in bug
        case T.stripPrefix "Unknown location; please try" rsp of
          Nothing -> False
          Just strippedRsp -> T.length strippedRsp < T.length rsp
      isImage = isJust . matchRegex (mkRegex ".png")
      getResponseData r =
        ( statusIsSuccessful $ responseStatus r,
          toStrict $ responseBody r
        )
   in do
        manager <- newManager defaultManagerSettings
        request <- parseRequest url
        (isOk, response) <-
          handle
            logException
            ( getResponseData
                <$> httpLbs
                  (request {requestHeaders = [("User-Agent", "curl")]})
                  manager
            )
        let body = decodeUtf8 response
        return $
          if not isOk || isImage url || unknownLocation body
            then "✨"
            else body

-- Logs an Http Exception and returns wttr.in's weather unknown label.
logException :: HttpException -> IO (Bool, ByteString)
logException e = do
  let errmsg = show e
  logM
    "System.Taffybar.Widget.WttrIn"
    ERROR
    ("Warning: Couldn't call wttr.in. \n" ++ errmsg)
  return (False, "✨")

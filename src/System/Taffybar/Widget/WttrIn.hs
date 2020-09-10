{-# LANGUAGE OverloadedStrings #-}
-- | This is a simple weather widget that polls wttr.in to retrieve the weather,
-- instead of relying on noaa data. 
--
-- Useful if NOAA data doesn't cover your needs, or if you just like wttr.in
-- better. 
--
-- For more information on how to use wttr.in, use:

module System.Taffybar.Widget.WttrIn ( textWttrNew ) where
import System.Log.Logger
import Control.Exception as E
import Control.Monad.IO.Class
import GI.Gtk
import qualified Data.Text as T
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Text.Regex
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.Taffybar.Widget.Generic.PollingLabel

-- | Creates a GTK Label widget that polls the requested wttr.in url for weather
-- information. 
--
-- Not compatible with image endpoints and binary data, such as the %.png%
-- endpoints. 
--
-- > -- Yields a label with the text "London: ⛅️  +72°F". Updates every 60
-- > -- seconds.
-- > textWttrNew "http://wttr.in/London?format=3" 60
textWttrNew 
 :: MonadIO m 
 => String -- ^ URL. All non-alphanumeric characters must be properly %-encoded.
 -> Double -- ^ Update Interval (in seconds)
 -> m Widget
textWttrNew url interval = pollingLabelNew interval (callWttr url)

-- | IO Action that calls wttr.in as per the user's request. 
callWttr :: String -> IO T.Text
callWttr url = do
  let unknownLocation rsp =
        case T.stripPrefix "Unknown location; please try" rsp of
          Nothing          -> False
          Just strippedRsp -> T.length strippedRsp < T.length rsp
      isImage = isJust . (matchRegex $ mkRegex ".png")
      getResponseData r = ( statusIsSuccessful $ responseStatus r
                          , toStrict $ responseBody r)
      catchAndLog = flip E.catch logException
  manager <- newManager defaultManagerSettings
  request <- parseRequest url
  (isOk, response) <- catchAndLog (getResponseData <$> httpLbs request manager) 
  let body = decodeUtf8 response
  if not isOk || isImage url || unknownLocation body
  then return $ "✨"
  else return $ body

-- Logs an Http Exception and returns wttr.in's weather unknown label.
logException :: HttpException -> IO (Bool, ByteString)
logException e = do
  let errmsg = show e
  logM "System.Taffybar.Widget.WttrIn" ERROR $
    ("Warning: Couldn't call wttr.in. \n" ++ errmsg)
  return $ (False, "✨")

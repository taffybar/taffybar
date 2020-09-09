{-# LANGUAGE OverloadedStrings #-}
-- | This is a simple weather widget that
-- polls wttr.in to retrieve the weather, instead
-- of relying on noaa data. 
--
-- Useful if NOAA data doesn't cover your needs,
-- or if you just like wttr.in better. 

module System.Taffybar.Widget.WttrIn ( textWttrNew ) where
import System.IO (hPutStrLn, stderr)
import Control.Exception as E
import Control.Monad.IO.Class
import GI.Gtk
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Network.HTTP.Client
import System.Taffybar.Widget.Generic.PollingLabel

-- | Creates a GTK Label widget that polls
-- the requested wttr.in url for weather
-- information. 
--
-- %url% /must/ be properly URL-encoded!
-- This means literal %'s must be used
-- as %25.
-- %interval% is the update interval. 
--
-- > -- Yields a label with the text "London: ⛅️  +72°F"
-- > textWttrNew "http://wttr.in/London?format=3"
textWttrNew :: MonadIO m => String -> Double -> m Widget
textWttrNew url interval = pollingLabelNew interval (callWttr url)

-- | IO Action that calls 
-- wttr.in as per the user's request. 
-- %url% /must/ be properly URL-encoded!
-- This means literal %'s must be used
-- as %25.
callWttr :: String -> IO T.Text
callWttr url = do
 let unknownLocation rsp = 
       case T.stripPrefix "Unknown location; please try" rsp of
         Nothing          -> False
         Just strippedRsp -> T.length strippedRsp < T.length rsp
 manager <- newManager defaultManagerSettings
 request <- parseRequest url
 response <- decodeUtf8 <$> E.catch (toStrict . responseBody <$> httpLbs request manager) logException
 if unknownLocation response
 then return $ "✨"
 else return $ response

-- Logs an Http Exception and returns wttr.in's weather unknown label.
logException :: HttpException -> IO ByteString
logException e = do
  let errmsg = show e
  hPutStrLn stderr ("Warning: Couldn't call wttr.in. \n" ++ errmsg)
  return $ encodeUtf8 "✨"


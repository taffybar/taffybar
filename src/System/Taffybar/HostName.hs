
module System.Taffybar.HostName
  ( hostNameNew
  , defaultHostNameConfig
  , HostNameConfig(..)
  ) where

import Data.Monoid ((<>))
import Graphics.UI.Gtk (Widget, toWidget, widgetShowAll)
import Network.HostName (getHostName)

import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)

-- | The configuration for the hostname label.
data HostNameConfig =
  HostNameConfig
    { hostNameFormatter :: String -> String
      -- ^ Format function for the hostname.  Takes the hostname as an argument
      -- and returns a hostname formatted with Pango markup.
      -- (default @\hostname -> \"\<span fgcolor=\'red\'\>\" \<\> hostname \<\> \"\</span\>\"@.
    }

defaultHostNameConfig :: HostNameConfig
defaultHostNameConfig =
  HostNameConfig
    { hostNameFormatter =
        \hostname -> "<span fgcolor='red'>" <> hostname <> "</span>"
    }

hostNameNew :: HostNameConfig -> IO Widget
hostNameNew cfg = do
  label  <- pollingLabelNew "" (60 * 60 * 24) $ getHostNameString cfg
  widgetShowAll label
  return $ toWidget label

getHostNameString :: HostNameConfig -> IO String
getHostNameString cfg = do
  hostName <- getHostName
  return $ hostNameFormatter cfg hostName

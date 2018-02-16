
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
    { hostNameBackgroundColor :: Maybe String -- ^ Background color. If
                                              -- 'Nothing', then no background
                                              -- color is specified. (default
                                              -- 'Nothing')
    , hostNameForegroundColor :: String -- ^ Foreground color. (default @\"red\"@)
    }

defaultHostNameConfig :: HostNameConfig
defaultHostNameConfig =
  HostNameConfig
    { hostNameBackgroundColor = Nothing
    , hostNameForegroundColor = "red"
    }

hostNameNew :: HostNameConfig -> IO Widget
hostNameNew cfg = do
  label  <- pollingLabelNew "" (60 * 60 * 24) $ getHostNameString cfg
  widgetShowAll label
  return $ toWidget label

getHostNameString :: HostNameConfig -> IO String
getHostNameString cfg = do
  hostName <- getHostName
  let bgColorFmt =
        case hostNameBackgroundColor cfg of
          Nothing -> ""
          Just bgColor -> "bgcolor='" <> bgColor <> "' "
  return $
    "<span fgcolor='" <>
    hostNameForegroundColor cfg <>
    "' " <>
    bgColorFmt <>
    ">" <>
    hostName <>
    "</span>"

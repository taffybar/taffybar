{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.Service where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           DBus
import           DBus.Client
import qualified DBus.TH as DBusTH
import qualified Data.ByteString as BS
import           Data.Int
import           Data.String
import qualified StatusNotifier.Watcher.Client as W

data ItemParams = ItemParams
  { iconName :: String
  , iconOverlayName :: String
  , itemDBusName :: String
  } deriving (Eq, Show, Read)

buildItem ItemParams
            { iconName = name
            , iconOverlayName = overlayName
            , itemDBusName = dbusName
            } = do
  client <- connectSession
  let
    getTooltip :: IO (String, [(Int32, Int32, BS.ByteString)], String, String)
    getTooltip = return ("", [], "Title", "Text")
  let clientInterface =
        Interface { interfaceName = "org.kde.StatusNotifierItem"
                  , interfaceMethods = []
                  , interfaceProperties =
                    [ readOnlyProperty "IconName" $ return name
                    , readOnlyProperty "OverlayIconName" $ return overlayName
                    , readOnlyProperty "ToolTip" $ getTooltip
                    ]
                  , interfaceSignals = []
                  }
  export client (fromString "/StatusNotifierItem") clientInterface
  requestName client (busName_ dbusName) []
  W.registerStatusNotifierItem client dbusName

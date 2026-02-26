{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.Client where

import DBus.Generation
import DBus.Internal.Types
import StatusNotifier.Util
import System.FilePath

generateClientFromFile
  defaultGenerationParams
  { genTakeSignalErrorHandler = True }
  False
  ("xml" </> "StatusNotifierItem.xml")

defaultPath :: ObjectPath
defaultPath = "/StatusNotifierItem"

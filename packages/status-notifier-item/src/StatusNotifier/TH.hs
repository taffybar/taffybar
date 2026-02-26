{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.TH where

import DBus.Client
import DBus.Generation

-- XXX: Move this to haskell-dbus
generateClient defaultGenerationParams $
               buildIntrospectionInterface $
               buildIntrospectableInterface undefined

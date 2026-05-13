{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (TaggedOverride (..), setupBinding)
import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.Gio.Config as Gio

main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where
    name = "Wp"
    version = "0.5"
    pkgName = "gi-wireplumber"
    pkgVersion = "0.5.14.2"
    overridesFile = Just "Wp.overrides"
    verbose = False
    outputDir = Nothing
    inheritedOverrides =
      [ TaggedOverride "inherited:GLib" GLib.overrides,
        TaggedOverride "inherited:GObject" GObject.overrides,
        TaggedOverride "inherited:Gio" Gio.overrides
      ]

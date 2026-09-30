module Main where

import GI.Gtk qualified as Gtk
import System.Environment (lookupEnv)
import System.Taffybar.Widget.Workspaces.LayoutSpec qualified as WorkspaceLayout
import Test.Hspec
import Test.Hspec.Runner
import TestLibSpec qualified
import UnitSpec qualified

main :: IO ()
main = do
  workspaceLayoutChild <- lookupEnv "TAFFYBAR_WORKSPACE_LAYOUT_CHILD"
  case workspaceLayoutChild of
    Just "1" -> do
      _ <- Gtk.init Nothing
      hspecWith defaultConfig WorkspaceLayout.gtkSpec
    _ -> hspecWith defaultConfig $ do
      UnitSpec.spec
      describe "testlib Sanity Checks" TestLibSpec.spec

module System.Taffybar.Widget.Workspaces.ChannelSpec (spec) where

import Data.Text qualified as T
import Data.Word (Word64)
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Widget.Workspaces.Channel (sortWindowsByPosition)
import Test.Hspec

mkX11Window :: Word64 -> Bool -> Maybe (Int, Int) -> WindowInfo
mkX11Window wid minimized pos =
  WindowInfo
    { windowIdentity = X11WindowIdentity wid,
      windowTitle = T.pack (show wid),
      windowClassHints = [],
      windowPosition = pos,
      windowUrgent = False,
      windowActive = False,
      windowMinimized = minimized
    }

mkWorkspace :: Int -> String -> WorkspaceViewState -> [WindowInfo] -> WorkspaceInfo
mkWorkspace idx name state windows =
  WorkspaceInfo
    { workspaceIdentity =
        WorkspaceIdentity
          { workspaceNumericId = Just idx,
            workspaceName = T.pack name
          },
      workspaceState = state,
      workspaceHasUrgentWindow = any windowUrgent windows,
      workspaceIsSpecial = False,
      workspaceWindows = windows
    }

mkSnapshot :: Word64 -> [WorkspaceInfo] -> WorkspaceSnapshot
mkSnapshot rev workspaces =
  WorkspaceSnapshot
    { snapshotBackend = WorkspaceBackendHyprland,
      snapshotRevision = rev,
      snapshotWindowDataComplete = True,
      snapshotWorkspaces = workspaces
    }

spec :: Spec
spec = do
  describe "sortWindowsByPosition" $ do
    it "orders non-minimized windows first, then by (x, y), then unknown positions" $ do
      let a = mkX11Window 1 False (Just (10, 0))
          b = mkX11Window 2 False (Just (0, 100))
          c = mkX11Window 3 True (Just (0, 0))
          d = mkX11Window 4 False Nothing
      sortWindowsByPosition [a, b, c, d] `shouldBe` [b, a, d, c]

  describe "diffWorkspaceSnapshots" $ do
    it "returns no events for identical snapshots" $ do
      let w1 = mkX11Window 1 False (Just (0, 0))
          ws1 = mkWorkspace 1 "1" WorkspaceActive [w1]
          snap = mkSnapshot 7 [ws1]
      diffWorkspaceSnapshots snap snap `shouldBe` []

    it "emits order/add/change/window events for meaningful updates" $ do
      let oldW1 = mkX11Window 1 False (Just (0, 0))
          newW1 = oldW1 {windowTitle = "updated"}
          newW2 = mkX11Window 2 False (Just (50, 50))
          oldWs1 = mkWorkspace 1 "1" WorkspaceActive [oldW1]
          newWs1 = mkWorkspace 1 "1" WorkspaceHidden [newW1, newW2]
          newWs2 = mkWorkspace 2 "2" WorkspaceVisible []
          oldSnap = mkSnapshot 10 [oldWs1]
          newSnap = mkSnapshot 11 [newWs2, newWs1]
          expectedEvents =
            [ WorkspaceOrderChanged [workspaceIdentity newWs2, workspaceIdentity newWs1],
              WorkspaceAdded newWs2,
              WorkspaceChanged newWs1,
              WorkspaceWindowsChanged
                (workspaceIdentity newWs1)
                [WindowAdded newW2, WindowChanged newW1]
            ]
      diffWorkspaceSnapshots oldSnap newSnap `shouldBe` expectedEvents

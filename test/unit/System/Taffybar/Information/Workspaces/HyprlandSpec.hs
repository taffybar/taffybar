module System.Taffybar.Information.Workspaces.HyprlandSpec (spec) where

import Data.Text qualified as T
import System.Taffybar.Information.Hyprland.Types
  ( HyprlandWorkspaceInfo (..),
  )
import System.Taffybar.Information.Workspaces.Hyprland
  ( applySpecialWorkspaceWindowTargets,
    sortHyprlandWorkspaces,
    specialWorkspaceWindowsToMinimized,
  )
import System.Taffybar.Information.Workspaces.Model
import Test.Hspec

mkWindow :: T.Text -> Bool -> WindowInfo
mkWindow title minimized =
  WindowInfo
    { windowIdentity = HyprlandWindowIdentity title,
      windowTitle = title,
      windowClassHints = [],
      windowPosition = Nothing,
      windowUrgent = False,
      windowActive = False,
      windowMinimized = minimized,
      windowPinned = False
    }

mkWorkspace ::
  Maybe Int ->
  T.Text ->
  WorkspaceViewState ->
  Bool ->
  [WindowInfo] ->
  WorkspaceInfo
mkWorkspace idx name state special windows =
  WorkspaceInfo
    { workspaceIdentity =
        WorkspaceIdentity
          { workspaceNumericId = idx,
            workspaceName = name
          },
      workspaceState = state,
      workspaceHasUrgentWindow = any windowUrgent windows,
      workspaceIsSpecial = special,
      workspaceWindows = windows
    }

workspaceByName :: T.Text -> [WorkspaceInfo] -> WorkspaceInfo
workspaceByName name workspaces =
  case filter ((== name) . workspaceName . workspaceIdentity) workspaces of
    [workspace] -> workspace
    [] -> error $ "Expected workspace named " <> T.unpack name
    _ -> error $ "Expected one workspace named " <> T.unpack name

spec :: Spec
spec = do
  describe "sortHyprlandWorkspaces" $
    it "orders normal numeric workspaces before special workspaces" $ do
      let specialMinimized = HyprlandWorkspaceInfo (-98) "special:minimized" Nothing (Just 1)
          specialScratch = HyprlandWorkspaceInfo (-99) "special:scratchpad" Nothing (Just 1)
          normal1 = HyprlandWorkspaceInfo 1 "1" (Just "DP-1") (Just 1)
          normal2 = HyprlandWorkspaceInfo 2 "2" (Just "DP-1") (Just 0)

      sortHyprlandWorkspaces [specialMinimized, normal2, specialScratch, normal1]
        `shouldBe` [normal1, normal2, specialScratch, specialMinimized]

  describe "applySpecialWorkspaceWindowTargets" $ do
    it "moves non-minimized special workspace windows into special:minimized" $ do
      let scratchWindow = mkWindow "scratch" False
          minimizedWindow = mkWindow "minimized" True
          scratch =
            mkWorkspace
              (Just (-99))
              "special:scratchpad"
              WorkspaceHidden
              True
              [scratchWindow]
          minimized =
            mkWorkspace
              (Just (-98))
              "special:minimized"
              WorkspaceHidden
              True
              [minimizedWindow]
          result =
            applySpecialWorkspaceWindowTargets
              specialWorkspaceWindowsToMinimized
              [scratch, minimized]
          movedScratch = workspaceByName "special:scratchpad" result
          minimizedBucket = workspaceByName "special:minimized" result
          movedWindow =
            case filter ((== HyprlandWindowIdentity "scratch") . windowIdentity) $
              workspaceWindows minimizedBucket of
              [window] -> window
              _ -> error "Expected scratch window in minimized bucket"

      workspaceWindows movedScratch `shouldBe` []
      workspaceState movedScratch `shouldBe` WorkspaceEmpty
      workspaceWindows minimizedBucket `shouldBe` [minimizedWindow, movedWindow]
      windowMinimized movedWindow `shouldBe` True

    it "creates a special:minimized bucket when the target workspace is missing" $ do
      let scratchWindow = mkWindow "scratch" False
          scratch =
            mkWorkspace
              (Just (-99))
              "special:scratchpad"
              WorkspaceHidden
              True
              [scratchWindow]
          result =
            applySpecialWorkspaceWindowTargets
              specialWorkspaceWindowsToMinimized
              [scratch]
          minimizedBucket = workspaceByName "special:minimized" result

      workspaceNumericId (workspaceIdentity minimizedBucket) `shouldBe` Nothing
      workspaceIsSpecial minimizedBucket `shouldBe` True
      workspaceState minimizedBucket `shouldBe` WorkspaceHidden
      map windowIdentity (workspaceWindows minimizedBucket)
        `shouldBe` [HyprlandWindowIdentity "scratch"]

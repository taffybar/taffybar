module System.Taffybar.Information.Workspaces.HyprlandSpec (spec) where

import Data.Text qualified as T
import System.Taffybar.Information.Workspaces.Hyprland
  ( applySpecialWorkspaceWindowTargets,
    sortWorkspaceInfos,
    specialWorkspaceWindowsToMinimized,
  )
import System.Taffybar.Information.Workspaces.Model
import Test.Hspec

mkWindow :: T.Text -> Bool -> WindowInfo
mkWindow title minimized =
  WindowInfo
    { windowIdentity = HyprlandWindowIdentity title,
      windowUpdateRevision = 0,
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
      workspaceUpdateRevision = 0,
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
  describe "sortWorkspaceInfos" $
    it "orders normal numeric workspaces before special workspaces" $ do
      let specialMinimized =
            mkWorkspace (Just (-98)) "special:minimized" WorkspaceHidden True []
          specialScratch =
            mkWorkspace (Just (-99)) "special:scratchpad" WorkspaceHidden True []
          normal1 =
            mkWorkspace (Just 1) "1" WorkspaceActive False []
          normal2 =
            mkWorkspace (Just 2) "2" WorkspaceEmpty False []

      sortWorkspaceInfos [specialMinimized, normal2, specialScratch, normal1]
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

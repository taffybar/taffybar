{-# OPTIONS_GHC -Wno-missing-fields #-}

module System.Taffybar.Widget.Workspaces.LayoutSpec (spec, gtkSpec) where

import Control.Concurrent.MVar qualified as MV
import Control.Exception (bracket)
import Control.Monad (filterM, forM_)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.GI.Base (castTo)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import GI.Gtk qualified as Gtk
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess)
import System.Taffybar.Context (Backend (..), Context (..))
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Test.UtilSpec (withSetEnv)
import System.Taffybar.Test.XvfbSpec (setDefaultDisplay_, withXvfb)
import System.Taffybar.Widget.Workspaces
import System.Taffybar.WindowIcon (pixBufFromColor)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  it "shows empty workspaces but hides special ones by default" $ do
    let shown = showWorkspaceFn defaultWorkspacesConfig
        empty = (workspace "5" []) {workspaceState = WorkspaceEmpty}
    shown empty `shouldBe` True
    shown empty {workspaceIsSpecial = True} `shouldBe` False
  layoutSpec

layoutSpec :: Spec
layoutSpec = aroundAll withXvfb $
  describe "workspace label layout" $
    it "reserves label space and preserves the opt-in overlay" $ \display ->
      setDefaultDisplay_ display $
        withSetEnv [("GDK_BACKEND", "x11"), ("TAFFYBAR_WORKSPACE_LAYOUT_CHILD", "1")] $ do
          executable <- getExecutablePath
          result <- timeout 20000000 $ readProcess (proc executable [])
          case result of
            Just (code, out, err) -> unlessSuccess code out err
            Nothing -> expectationFailure "Workspace layout subprocess timed out"
  where
    unlessSuccess ExitSuccess _ _ = pure ()
    unlessSuccess code out err = expectationFailure $ show code ++ "\n" ++ BL.unpack out ++ "\n" ++ BL.unpack err

gtkSpec :: Spec
gtkSpec = sequential $ describe "workspace label layout" $ do
  forM_ ["1", "a long workspace name"] $ \name ->
    it ("places " ++ T.unpack name ++ " before the icons by default") $
      withController defaultWorkspacesConfig (workspace name [window 1, window 2]) $ \_ controller ->
        assertLabelSpace (controllerWidget controller)

  it "keeps the label visible on an empty workspace and separates newly added icons" $
    withController defaultWorkspacesConfig (workspace "1" []) $ \ctx controller -> do
      let root = controllerWidget controller
      [label] <- widgetsWithClass "workspace-label" root
      (labelWidth, _) <- Gtk.widgetGetPreferredWidth label
      labelWidth `shouldSatisfy` (> 0)
      (rootWidth, _) <- Gtk.widgetGetPreferredWidth root
      rootWidth `shouldSatisfy` (>= labelWidth)
      length <$> widgetsWithClass "window-icon-container" root `shouldReturn` 0
      runReaderT (controllerUpdate controller $ workspace "longer label" [window 1]) ctx
      Gtk.widgetShowAll root
      assertLabelSpace root
      runReaderT (controllerUpdate controller $ workspace "1" []) ctx
      icons <- widgetsWithClass "window-icon-container" root
      length <$> filterM Gtk.widgetGetVisible icons `shouldReturn` 0

  it "preserves the opt-in overlay and lets input pass through the label"
    $ withController
      defaultWorkspacesConfig {widgetBuilder = labelOverlayWidgetBuilder}
      (workspace "1" [window 1])
    $ \_ controller -> do
      let root = controllerWidget controller
      Just overlay <- castTo Gtk.Overlay root
      [labelBox] <- widgetsWithClass "overlay-box" root
      Gtk.overlayGetOverlayPassThrough overlay labelBox `shouldReturn` True

withController ::
  WorkspacesConfig ->
  WorkspaceInfo ->
  (Context -> WorkspaceWidgetController -> IO ()) ->
  IO ()
withController cfg ws action = do
  state <- MV.newMVar M.empty
  let ctx = Context {contextState = state, backend = BackendX11}
      testCfg = cfg {getWindowIconPixbuf = \size _ -> Just <$> pixBufFromColor size 0xff0000ff}
  bracket
    (runReaderT (widgetBuilder testCfg testCfg ws) ctx)
    (Gtk.widgetDestroy . controllerWidget)
    $ \controller -> do
      Gtk.widgetShowAll (controllerWidget controller)
      action ctx controller

widgetsWithClass :: T.Text -> Gtk.Widget -> IO [Gtk.Widget]
widgetsWithClass cssClass root = do
  style <- Gtk.widgetGetStyleContext root
  matches <- Gtk.styleContextHasClass style cssClass
  container <- castTo Gtk.Container root
  children <- maybe (pure []) Gtk.containerGetChildren container
  descendants <- concat <$> mapM (widgetsWithClass cssClass) children
  pure $ [root | matches] ++ descendants

assertLabelSpace :: Gtk.Widget -> Expectation
assertLabelSpace root = do
  [label] <- widgetsWithClass "workspace-label" root
  icons <- widgetsWithClass "window-icon-container" root
  length icons `shouldSatisfy` (> 0)
  (labelWidth, _) <- Gtk.widgetGetPreferredWidth label
  labelWidth `shouldSatisfy` (> 0)
  iconWidths <- mapM (fmap fst . Gtk.widgetGetPreferredWidth) icons
  (rootWidth, _) <- Gtk.widgetGetPreferredWidth root
  rootWidth `shouldSatisfy` (>= labelWidth + sum iconWidths)
  Just labelParent <- Gtk.widgetGetParent label
  Just box <- castTo Gtk.Box labelParent
  children <- Gtk.containerGetChildren box
  mapM Gtk.widgetGetName (take 1 children) `shouldReturn` ["GtkLabel"]
  forM_ [root, label] $ \widget -> do
    style <- Gtk.widgetGetStyleContext widget
    Gtk.styleContextHasClass style "active" `shouldReturn` True

workspace :: T.Text -> [WindowInfo] -> WorkspaceInfo
workspace name windows =
  WorkspaceInfo
    { workspaceIdentity = WorkspaceIdentity (Just 1) name,
      workspaceUpdateRevision = 0,
      workspaceState = WorkspaceActive,
      workspaceHasUrgentWindow = False,
      workspaceIsSpecial = False,
      workspaceWindows = windows
    }

window :: Word -> WindowInfo
window wid =
  WindowInfo
    { windowIdentity = X11WindowIdentity (fromIntegral wid),
      windowUpdateRevision = 0,
      windowTitle = "test window",
      windowClassHints = [],
      windowPosition = Nothing,
      windowUrgent = False,
      windowActive = False,
      windowMinimized = False,
      windowPinned = False
    }

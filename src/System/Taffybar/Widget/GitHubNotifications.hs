module System.Taffybar.Widget.GitHubNotifications where

import           Control.Concurrent
import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import qualified GI.Gtk as Gtk
import qualified GitHub.Auth as Auth
import           GitHub.Data
import           GitHub.Endpoints.Activity.Notifications
import qualified Graphics.UI.Gtk as Gtk2hs
import           System.FilePath
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.DynamicMenu
import           Text.Printf

import Paths_taffybar ( getDataDir )

data GitHubConfig = GitHubConfig
  { ghAuth :: Auth.Auth
  , ghGetLabelText :: Either Error (V.Vector Notification) -> T.Text
  , ghGetPixbuf :: Either Error (V.Vector Notification) -> IO PB.Pixbuf
  , ghRefreshSeconds :: Rational
  }

errorOrNotifsLength :: (Show a1, Foldable t) => Either a1 (t a2) -> String
errorOrNotifsLength (Right notifs) = show $ length notifs
errorOrNotifsLength (Left e) = show e

defaultGithubConfig :: Auth -> GitHubConfig
defaultGithubConfig auth = GitHubConfig
  { ghAuth = auth
  , ghGetLabelText = T.pack . errorOrNotifsLength
  , ghGetPixbuf = const $ ((</> "icons" </> "github.svg") <$> getDataDir) >>= PB.pixbufNewFromFile
  , ghRefreshSeconds = 15
  }

githubNotificationsNew :: MonadIO m => GitHubConfig -> m Gtk2hs.Widget
githubNotificationsNew GitHubConfig { ghAuth = auth
                                    , ghGetLabelText = getLabelTextFromNotifs
                                    , ghRefreshSeconds = refreshSeconds
                                    , ghGetPixbuf = getPixbuf
                                    } = liftIO $ fromGIWidget =<< do
  let getAllNotifications = getNotifications auth
  notificationsVar <- getAllNotifications >>= MV.newMVar
  let getLabelText = getLabelTextFromNotifs <$> MV.readMVar notificationsVar

  grid <- Gtk.gridNew
  label <- (Just <$> getLabelText) >>= Gtk.labelNew
  _ <- Gtk.onWidgetRealize label $ do
    refreshThreadID <- foreverWithDelay refreshSeconds $ do
      newNotifications <- getAllNotifications
      void $ MV.swapMVar notificationsVar newNotifications
      runOnUIThread $ getLabelText >>= Gtk.labelSetMarkup label
    void $ Gtk.onWidgetDestroy label $ killThread refreshThreadID
    return ()
  image <- MV.readMVar notificationsVar >>=
           getPixbuf >>=
           Gtk.imageNewFromPixbuf . Just

  Gtk.containerAdd grid image
  Gtk.containerAdd grid label
  Gtk.gridSetColumnSpacing grid 3
  Gtk.widgetShowAll grid
  widget <- Gtk.toWidget grid

  let getNotificationsVector =
        (either (const V.empty) id <$> MV.readMVar notificationsVar)
      populateMenu menu = do
        notifications <- getNotificationsVector
        mapM_ ((>>= Gtk.menuShellAppend menu) . makeNotificationItem) notifications
        Gtk.widgetShowAll menu
      dynamicMenuConfig =
        DynamicMenuConfig
          { dmClickWidget = widget
          , dmPopulateMenu = populateMenu
          }

  dynamicMenuNew dynamicMenuConfig

makeNotificationItem :: MonadIO m => Notification -> m Gtk.MenuItem
makeNotificationItem Notification { notificationSubject = Subject { subjectTitle = title
                                                                  , subjectURL = url
                                                                  }
                                  , notificationRepo = RepoRef {repoRefRepo = repoNombre}
                                  } = do
  menuItem <- Gtk.menuItemNewWithLabel $ T.pack $ printf "%s - %s"
              (untagName repoNombre) title
  _ <- Gtk.onWidgetButtonPressEvent menuItem $ const $
       runCommandFromPath ["xdg-open", T.unpack $ getUrl url] >> return True
  return menuItem

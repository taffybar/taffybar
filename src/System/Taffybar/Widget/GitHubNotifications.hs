{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Widget.GitHubNotifications where

import           Control.Concurrent
import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import qualified GI.Gtk as Gtk
import qualified GitHub.Auth as Auth
import           GitHub.Data
import           GitHub.Endpoints.Activity.Notifications
import qualified Graphics.UI.Gtk as Gtk2hs
import           Network.HTTP.Simple
import           System.Log.Logger
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.DynamicMenu
import           System.Taffybar.Widget.Util
import           Text.Printf

ghLog :: Priority -> String -> IO ()
ghLog = logM "System.Taffybar.Widget.GitHubNotifications"

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
  , ghGetPixbuf = const $ loadIcon 20 "github.svg"
  , ghRefreshSeconds = 15
  }

githubNotificationsNew :: MonadIO m => GitHubConfig -> m Gtk2hs.Widget
githubNotificationsNew config@GitHubConfig
                         { ghAuth = auth
                         , ghGetLabelText = getLabelTextFromNotifs
                         , ghRefreshSeconds = refreshSeconds
                         , ghGetPixbuf = getPixbuf
                         } = liftIO $ fromGIWidget =<< do
  let getAllNotifications = getNotifications auth
      logAndShow :: (Show v) => Priority -> String -> v -> IO ()
      logAndShow level message value =
        ghLog level $ printf message (show value)
  notificationsVar <- getAllNotifications >>= MV.newMVar
  let getLabelText = getLabelTextFromNotifs <$> MV.readMVar notificationsVar

  grid <- Gtk.gridNew
  label <- (Just <$> getLabelText) >>= Gtk.labelNew
  _ <- Gtk.onWidgetRealize label $ do
    refreshThreadID <- foreverWithDelay refreshSeconds $ do
      newNotifications <- getAllNotifications
      either (logAndShow WARNING "Got error fetching github notifications %s")
             ((logAndShow DEBUG "Got %s notifications from github") . V.length)
             newNotifications
      void $ MV.swapMVar notificationsVar newNotifications
      runOnUIThread $ getLabelText >>= Gtk.labelSetMarkup label
    void $ Gtk.onWidgetDestroy label $ killThread refreshThreadID
    return ()
  image <- MV.readMVar notificationsVar >>=
           getPixbuf >>=
           Gtk.imageNewFromPixbuf . Just

  Gtk.containerAdd grid image
  Gtk.containerAdd grid label
  -- TODO: make spacing configurable
  Gtk.gridSetColumnSpacing grid 3
  Gtk.setWidgetValign grid Gtk.AlignCenter
  Gtk.setWidgetHalign grid Gtk.AlignCenter
  Gtk.widgetShowAll grid
  widget <- Gtk.toWidget grid

  let getNotificationsVector =
        (either (const V.empty) id <$> MV.readMVar notificationsVar)
      populateMenu menu = do
        let addToMenu :: Gtk.IsMenuItem b => b -> IO ()
            addToMenu = Gtk.menuShellAppend menu
        notifications <- getNotificationsVector
        mapM_ ((>>= addToMenu) . makeNotificationItem config) notifications
        Gtk.separatorMenuItemNew >>= addToMenu

        viewAll <- Gtk.menuItemNewWithLabel (T.pack "View All")
        _ <- Gtk.onWidgetButtonPressEvent viewAll $ const $
             openURL "https://github.com/notifications" >>
             return True
        addToMenu viewAll

        markAllAsRead <- Gtk.menuItemNewWithLabel (T.pack "Mark All As Read")
        _ <- Gtk.onWidgetButtonPressEvent markAllAsRead $ const $
             markNotificationsAsRead auth >>=
             logAndShow DEBUG "Mark all read response: %s" >>
             return True
        addToMenu markAllAsRead

        Gtk.widgetShowAll menu
      dynamicMenuConfig =
        DynamicMenuConfig
          { dmClickWidget = widget
          , dmPopulateMenu = populateMenu
          }

  dynamicMenuNew dynamicMenuConfig

openNotificationHTML :: Notification -> IO ()
openNotificationHTML notification = do
  let setUserAgent = setRequestHeader
                   "User-Agent" ["Taffybar-GithubNotifier"]
      request = setUserAgent $ parseRequest_ $ T.unpack $ getUrl $
                subjectURL $ notificationSubject notification
  response <- httpLBS request
  ghLog DEBUG $ printf "Got response from subject url: %s" $ show response
  let maybeUrl = getHTMLURL $ getResponseBody response
  void $ sequenceA $ openURL . T.unpack <$> maybeUrl
  return ()

getHTMLURL :: LBS.ByteString -> Maybe T.Text
getHTMLURL jsonText = decode jsonText >>= parseMaybe (.: "html_url")

makeNotificationItem :: MonadIO m => GitHubConfig -> Notification -> m Gtk.MenuItem
makeNotificationItem GitHubConfig { ghAuth = auth }
                     notification@Notification
                       { notificationId = thisNotificationId
                       , notificationSubject = Subject
                         { subjectTitle = title }
                       , notificationRepo = RepoRef
                         { repoRefRepo = repositoryName }
                       } = do
  let notificationText = T.pack $ printf "%s - %s"
                         (untagName repositoryName) title
      openHTML = openNotificationHTML notification >> return True
  menuItem <- Gtk.menuItemNewWithLabel $ notificationText
  _ <- Gtk.onWidgetButtonPressEvent menuItem $ const openHTML

  submenu <- Gtk.menuNew
  markAsReadItem <- Gtk.menuItemNewWithLabel $ "Mark as read"
  _ <- Gtk.onWidgetButtonPressEvent markAsReadItem $ const $
       markNotificationAsRead auth thisNotificationId >> return True
  viewItem <- Gtk.menuItemNewWithLabel $ "View on GitHub"
  _ <- Gtk.onWidgetButtonPressEvent viewItem $ const openHTML

  Gtk.menuShellAppend submenu markAsReadItem
  Gtk.menuShellAppend submenu viewItem

  Gtk.menuItemSetSubmenu menuItem $ Just submenu

  return menuItem

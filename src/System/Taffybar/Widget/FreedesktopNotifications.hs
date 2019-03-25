{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This widget listens on DBus for freedesktop notifications
-- (<http://developer.gnome.org/notification-spec/>).  Currently it is
-- somewhat ugly, but the format is somewhat configurable.  A visual
-- overhaul of the widget is coming.
--
-- The widget only displays one notification at a time and
-- notifications are cancellable.

-- The notificationDaemon thread handles new notifications
-- and cancellation requests, adding or removing the notification
-- to or from the queue. It additionally starts a timeout thread
-- for each notification added to queue.
--
-- The display thread blocks idling until it is awakened to refresh the GUI
--
-- A timeout thread is associated with a notification id.
-- It sleeps until the specific timeout and then removes every notification
-- with that id from the queue

module System.Taffybar.Widget.FreedesktopNotifications
  ( Notification(..)
  , NotificationConfig(..)
  , defaultNotificationConfig
  , notifyAreaNew
  ) where

import           BroadcastChan
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad ( forever, void )
import           Control.Monad.IO.Class
import           DBus
import           DBus.Client
import           Data.Foldable
import           Data.Int ( Int32 )
import           Data.Map ( Map )
import           Data.Monoid
import           Data.Sequence ( Seq, (|>), viewl, ViewL(..) )
import qualified Data.Sequence as S
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Word ( Word32 )
import           GI.GLib (markupEscapeText)
import           GI.Gtk
import qualified GI.Pango as Pango
import           System.Taffybar.Util

-- | A simple structure representing a Freedesktop notification
data Notification = Notification
  { noteAppName :: Text
  , noteReplaceId :: Word32
  , noteSummary :: Text
  , noteBody :: Text
  , noteExpireTimeout :: Maybe Int32
  , noteId :: Word32
  } deriving (Show, Eq)

data NotifyState = NotifyState
  { noteWidget :: Label
  , noteContainer :: Widget
  , noteConfig :: NotificationConfig -- ^ The associated configuration
  , noteQueue :: TVar (Seq Notification) -- ^ The queue of active notifications
  , noteIdSource :: TVar Word32 -- ^ A source of fresh notification ids
  , noteChan :: BroadcastChan In () -- ^ Writing to this channel wakes up the display thread
  }

initialNoteState :: Widget -> Label -> NotificationConfig -> IO NotifyState
initialNoteState wrapper l cfg = do
  m <- newTVarIO 1
  q <- newTVarIO S.empty
  ch <- newBroadcastChan
  return NotifyState { noteQueue = q
                     , noteIdSource = m
                     , noteWidget = l
                     , noteContainer = wrapper
                     , noteConfig = cfg
                     , noteChan = ch
                     }

-- | Removes every notification with id 'nId' from the queue
notePurge :: NotifyState -> Word32 -> IO ()
notePurge s nId = atomically . modifyTVar' (noteQueue s) $
  S.filter ((nId /=) . noteId)

-- | Removes the first (oldest) notification from the queue
noteNext :: NotifyState -> IO ()
noteNext s = atomically $ modifyTVar' (noteQueue s) aux
  where
    aux queue = case viewl queue of
      EmptyL -> S.empty
      _ :< ns -> ns

-- | Generates a fresh notification id
noteFreshId :: NotifyState -> IO Word32
noteFreshId NotifyState { noteIdSource } = atomically $ do
  nId <- readTVar noteIdSource
  writeTVar noteIdSource (succ nId)
  return nId

--------------------------------------------------------------------------------
-- | Handles a new notification
notify :: NotifyState
       -> Text -- ^ Application name
       -> Word32 -- ^ Replaces id
       -> Text -- ^ App icon
       -> Text -- ^ Summary
       -> Text -- ^ Body
       -> [Text] -- ^ Actions
       -> Map Text Variant -- ^ Hints
       -> Int32 -- ^ Expires timeout (milliseconds)
       -> IO Word32
notify s appName replaceId _ summary body _ _ timeout = do
  realId <- if replaceId == 0 then noteFreshId s else return replaceId
  let configTimeout = notificationMaxTimeout (noteConfig s)
      realTimeout = if timeout <= 0 -- Gracefully handle out of spec negative values
                    then configTimeout
                    else case configTimeout of
                           Nothing -> Just timeout
                           Just maxTimeout -> Just (min maxTimeout timeout)

  escapedSummary <- markupEscapeText summary (-1)
  escapedBody <- markupEscapeText body (-1)
  let n = Notification { noteAppName = appName
                       , noteReplaceId = replaceId
                       , noteSummary = escapedSummary
                       , noteBody = escapedBody
                       , noteExpireTimeout = realTimeout
                       , noteId = realId
                       }
  -- Either add the new note to the queue or replace an existing note if their ids match
  atomically $ do
    queue <- readTVar $ noteQueue s
    writeTVar (noteQueue s) $ case S.findIndexL (\n_ -> noteId n == noteId n_) queue of
      Nothing -> queue |> n
      Just index -> S.update index n queue
  startTimeoutThread s n
  wakeupDisplayThread s
  return realId

-- | Handles user cancellation of a notification
closeNotification :: NotifyState -> Word32 -> IO ()
closeNotification s nId = do
  notePurge s nId
  wakeupDisplayThread s

notificationDaemon :: (AutoMethod f1, AutoMethod f2)
                      => f1 -> f2 -> IO ()
notificationDaemon onNote onCloseNote = do
  client <- connectSession
  _ <- requestName client "org.freedesktop.Notifications" [nameAllowReplacement, nameReplaceExisting]
  export client "/org/freedesktop/Notifications" interface
  where
    getServerInformation :: IO (Text, Text, Text, Text)
    getServerInformation = return ("haskell-notification-daemon",
                                   "nochair.net",
                                   "0.0.1",
                                   "1.1")
    getCapabilities :: IO [Text]
    getCapabilities = return ["body", "body-markup"]
    interface = defaultInterface
      { interfaceName = "org.freedesktop.Notifications"
      , interfaceMethods =
          [ autoMethod "GetServerInformation" getServerInformation
          , autoMethod "GetCapabilities" getCapabilities
          , autoMethod "CloseNotification" onCloseNote
          , autoMethod "Notify" onNote
          ]
      }

--------------------------------------------------------------------------------
wakeupDisplayThread :: NotifyState -> IO ()
wakeupDisplayThread s = void $ writeBChan (noteChan s) ()

-- | Refreshes the GUI
displayThread :: NotifyState -> IO ()
displayThread s = do
  chan <- newBChanListener (noteChan s)
  forever $ do
    _ <- readBChan chan
    ns <- readTVarIO (noteQueue s)
    postGUIASync $
      if S.length ns == 0
      then widgetHide (noteContainer s)
      else do
        labelSetMarkup (noteWidget s) $ formatMessage (noteConfig s) (toList ns)
        widgetShowAll (noteContainer s)
  where
    formatMessage NotificationConfig {..} ns =
      T.take notificationMaxLength $ notificationFormatter ns

--------------------------------------------------------------------------------
startTimeoutThread :: NotifyState -> Notification -> IO ()
startTimeoutThread s Notification {..} = case noteExpireTimeout of
  Nothing -> return ()
  Just timeout -> void $ forkIO $ do
    threadDelay (fromIntegral timeout * 10^(3 :: Int))
    notePurge s noteId
    wakeupDisplayThread s

--------------------------------------------------------------------------------
data NotificationConfig = NotificationConfig
  { notificationMaxTimeout :: Maybe Int32 -- ^ Maximum time that a notification will be displayed (in seconds).  Default: None
  , notificationMaxLength :: Int -- ^ Maximum length displayed, in characters.  Default: 100
  , notificationFormatter :: [Notification] -> T.Text -- ^ Function used to format notifications, takes the notifications from first to last
  }

defaultFormatter :: [Notification] -> T.Text
defaultFormatter ns =
  let count = length ns
      n = head ns
      prefix = if count == 1
               then ""
               else "(" <> T.pack (show count) <> ") "
      msg =  if T.null (noteBody n)
             then noteSummary n
             else noteSummary n <> ": " <> noteBody n
  in "<span fgcolor='yellow'>" <> prefix <> "</span>" <> msg

-- | The default formatter is one of
-- * Summary : Body
-- * Summary
-- * (N) Summary : Body
-- * (N) Summary
-- depending on the presence of a notification body, and where N is the number of queued notifications.
defaultNotificationConfig :: NotificationConfig
defaultNotificationConfig =
  NotificationConfig { notificationMaxTimeout = Nothing
                     , notificationMaxLength = 100
                     , notificationFormatter = defaultFormatter
                     }

-- | Create a new notification area with the given configuration.
notifyAreaNew :: MonadIO m => NotificationConfig -> m Widget
notifyAreaNew cfg = liftIO $ do
  frame <- frameNew Nothing
  box <- boxNew OrientationHorizontal 3
  textArea <- labelNew (Nothing :: Maybe Text)
  button <- eventBoxNew
  sep <- separatorNew OrientationHorizontal

  bLabel <- labelNew (Nothing :: Maybe Text)
  widgetSetName bLabel "NotificationCloseButton"
  labelSetMarkup bLabel "×"

  labelSetMaxWidthChars textArea (fromIntegral $ notificationMaxLength cfg)
  labelSetEllipsize textArea Pango.EllipsizeModeEnd

  containerAdd button bLabel
  boxPackStart box textArea True True 0
  boxPackStart box sep False False 0
  boxPackStart box button False False 0

  containerAdd frame box

  widgetHide frame
  w <- toWidget frame

  s <- initialNoteState w textArea cfg
  _ <- onWidgetButtonReleaseEvent button (userCancel s)

  realizableWrapper <- boxNew OrientationHorizontal 0
  boxPackStart realizableWrapper frame False False 0
  widgetShow realizableWrapper

  -- We can't start the dbus listener thread until we are in the GTK
  -- main loop, otherwise things are prone to lock up and block
  -- infinitely on an mvar.  Bad stuff - only start the dbus thread
  -- after the fake invisible wrapper widget is realized.
  void $ onWidgetRealize realizableWrapper $ do
    void $ forkIO (displayThread s)
    notificationDaemon (notify s) (closeNotification s)

  -- Don't show the widget by default - it will appear when needed
  toWidget realizableWrapper

  where
    -- | Close the current note and pull up the next, if any
    userCancel s _ = do
      noteNext s
      wakeupDisplayThread s
      return True

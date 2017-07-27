{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This widget listens on DBus for freedesktop notifications
-- (http://developer.gnome.org/notification-spec/).  Currently it is
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

module System.Taffybar.FreedesktopNotifications (
  -- * Types
  Notification(..),
  NotificationConfig(..),
  -- * Constructor
  notifyAreaNew,
  defaultNotificationConfig
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad ( forever, void )
import Control.Monad.Trans ( liftIO )
import Data.Int ( Int32 )
import Data.Foldable
import Data.Map ( Map )
import Data.Monoid
import qualified Data.Sequence as S
import Data.Sequence ( Seq, (|>), viewl, ViewL(..) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Word ( Word32 )
import DBus
import DBus.Client
import Graphics.UI.Gtk hiding ( Variant )

-- | A simple structure representing a Freedesktop notification
data Notification = Notification { noteAppName :: Text
                                 , noteReplaceId :: Word32
                                 , noteSummary :: Text
                                 , noteBody :: Text
                                 , noteExpireTimeout :: Maybe Int32
                                 , noteId :: Word32
                                 }
                    deriving (Show, Eq)

data NotifyState = NotifyState { noteWidget :: Label
                               , noteContainer :: Widget
                               , noteConfig :: NotificationConfig
                                 -- ^ The associated configuration
                               , noteQueue :: TVar (Seq Notification)
                                 -- ^ The queue of active notifications
                               , noteIdSource :: TVar Word32
                                 -- ^ A source of fresh notification ids
                               , noteChan :: Chan ()
                                 -- ^ Writing to this channel wakes up the display thread
                               }

initialNoteState :: Widget -> Label -> NotificationConfig -> IO NotifyState
initialNoteState wrapper l cfg = do
  m <- newTVarIO 1
  q <- newTVarIO S.empty
  ch <- newChan
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
noteFreshId (NotifyState { noteIdSource }) = atomically $ do
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
  realId <- if replaceId == 0 then noteFreshId s else pure replaceId
  let escapeText = T.pack . escapeMarkup . T.unpack
      configTimeout = notificationMaxTimeout (noteConfig s)
      realTimeout = if timeout <= 0 -- Gracefully handle out of spec negative values
                    then configTimeout
                    else case configTimeout of
                           Nothing -> Just timeout
                           Just maxTimeout -> Just (min maxTimeout timeout)
      n = Notification { noteAppName = appName
                       , noteReplaceId = replaceId
                       , noteSummary = escapeText summary
                       , noteBody = escapeText body
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
  requestName client "org.freedesktop.Notifications" [nameAllowReplacement, nameReplaceExisting]
  export client "/org/freedesktop/Notifications" interface
  where
    getServerInformation :: IO (Text, Text, Text, Text)
    getServerInformation = pure ("haskell-notification-daemon",
                                 "nochair.net",
                                 "0.0.1",
                                 "1.1")
    getCapabilities :: IO [Text]
    getCapabilities = pure ["body", "body-markup"]
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
wakeupDisplayThread s = writeChan (noteChan s) ()

-- | Refreshes the GUI
displayThread :: NotifyState -> IO ()
displayThread s = forever $ do
  () <- readChan (noteChan s)
  ns <- readTVarIO (noteQueue s)
  postGUIAsync $
    if S.length ns == 0
    then widgetHideAll (noteContainer s)
    else do
      labelSetMarkup (noteWidget s) $ formatMessage (noteConfig s) (toList ns)
      widgetShowAll (noteContainer s)
  where
    formatMessage (NotificationConfig {..}) ns =
      take notificationMaxLength $ notificationFormatter ns

--------------------------------------------------------------------------------
startTimeoutThread :: NotifyState -> Notification -> IO ()
startTimeoutThread s (Notification {..}) = case noteExpireTimeout of
  Nothing -> return ()
  Just timeout -> void $ forkIO $ do
    threadDelay (fromIntegral timeout * 10^(6 :: Int))
    notePurge s noteId
    wakeupDisplayThread s

--------------------------------------------------------------------------------
data NotificationConfig =
  NotificationConfig { notificationMaxTimeout :: Maybe Int32 -- ^ Maximum time that a notification will be displayed (in seconds).  Default: None
                     , notificationMaxLength :: Int  -- ^ Maximum length displayed, in characters.  Default: 100
                     , notificationFormatter :: [Notification] -> String -- ^ Function used to format notifications, takes the notifications from first to last
                     }

defaultFormatter :: [Notification] -> String
defaultFormatter ns =
  let count = length ns
      n = head ns
      prefix = if count == 1
               then ""
               else "(" <> show count <> ") "
      msg = T.unpack $ if T.null (noteBody n)
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
notifyAreaNew :: NotificationConfig -> IO Widget
notifyAreaNew cfg = do
  frame <- frameNew
  box <- hBoxNew False 3
  textArea <- labelNew (Nothing :: Maybe String)
  button <- eventBoxNew
  sep <- vSeparatorNew

  bLabel <- labelNew (Nothing :: Maybe String)
  widgetSetName bLabel ("NotificationCloseButton" :: String)
  labelSetMarkup bLabel ("×" :: String)

  labelSetMaxWidthChars textArea (notificationMaxLength cfg)
  labelSetEllipsize textArea EllipsizeEnd

  containerAdd button bLabel
  boxPackStart box textArea PackGrow 0
  boxPackStart box sep PackNatural 0
  boxPackStart box button PackNatural 0

  containerAdd frame box

  widgetHide frame

  s <- initialNoteState (toWidget frame) textArea cfg
  _ <- on button buttonReleaseEvent (userCancel s)

  realizableWrapper <- hBoxNew False 0
  boxPackStart realizableWrapper frame PackNatural 0
  widgetShow realizableWrapper

  -- We can't start the dbus listener thread until we are in the GTK
  -- main loop, otherwise things are prone to lock up and block
  -- infinitely on an mvar.  Bad stuff - only start the dbus thread
  -- after the fake invisible wrapper widget is realized.
  void $ on realizableWrapper realize $ do
    void $ forkIO (displayThread s)
    notificationDaemon (notify s) (closeNotification s)

  -- Don't show the widget by default - it will appear when needed
  return (toWidget realizableWrapper)
  where
    -- | Close the current note and pull up the next, if any
    userCancel s = liftIO $ do
      noteNext s
      wakeupDisplayThread s
      return True

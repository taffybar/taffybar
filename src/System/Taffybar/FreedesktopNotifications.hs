{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This widget listens on DBus for freedesktop notifications
-- (http://developer.gnome.org/notification-spec/).  Currently it is
-- somewhat ugly, but the format is somewhat configurable.  A visual
-- overhaul of the widget is coming.
--
-- The widget only displays one notification at a time and
-- notifications are cancellable.
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
import Control.Monad ( forever )
import Control.Monad.Trans ( liftIO )
import Data.Int ( Int32 )
import Data.Map ( Map )
import Data.Monoid ( mconcat )
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
                                 , noteExpireTimeout :: Int32
                                 , noteId :: Word32
                                 }
                    deriving (Show, Eq)

data NotifyState = NotifyState { noteWidget :: Label
                               , noteContainer :: Widget
                               , noteConfig :: NotificationConfig
                               , noteQueue :: TVar (Seq Notification)
                                 -- ^ The queue of active (but not yet
                                 -- displayed) notifications
                               , noteIdSource :: TVar Word32
                                 -- ^ A source of new notification ids
                               , noteCurrent :: TVar (Maybe Notification)
                                 -- ^ The current note being displayed
                               , noteChan :: Chan ()
                                 -- ^ Wakes up the GUI update thread
                               }

initialNoteState :: Widget -> Label -> NotificationConfig -> IO NotifyState
initialNoteState wrapper l cfg = do
  m <- newTVarIO 1
  q <- newTVarIO S.empty
  c <- newTVarIO Nothing
  ch <- newChan
  return NotifyState { noteQueue = q
                     , noteIdSource = m
                     , noteWidget = l
                     , noteContainer = wrapper
                     , noteCurrent = c
                     , noteConfig = cfg
                     , noteChan = ch
                     }

getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return ("haskell-notification-daemon",
          "nochair.net",
          "0.0.1",
          "1.1")

getCapabilities :: IO [Text]
getCapabilities = return ["body", "body-markup"]

nextNotification :: NotifyState -> STM ()
nextNotification s = do
  q <- readTVar (noteQueue s)
  case viewl q of
    EmptyL -> do
      writeTVar (noteCurrent s) Nothing
    next :< rest -> do
      writeTVar (noteQueue s) rest
      writeTVar (noteCurrent s) (Just next)

-- | Filter any notifications with this id from the current queue.  If
-- it is the current notification, replace it with the next, if any.
closeNotification :: NotifyState -> Word32 -> IO ()
closeNotification istate nid = do
  atomically $ do
    modifyTVar' (noteQueue istate) removeNote
    curNote <- readTVar (noteCurrent istate)
    case curNote of
      Nothing -> return ()
      Just cnote
        | noteId cnote /= nid -> return ()
        | otherwise ->
          -- in this case, the note was current so we take the next,
          -- if any
          nextNotification istate
  wakeupDisplayThread istate
  where
    removeNote = S.filter (\n -> noteId n /= nid)

-- | Apply the user's formatter and truncate the result with the
-- specified maxlen.
formatMessage :: NotifyState -> Notification -> String
formatMessage s = fmt
  where
    fmt = notificationFormatter $ noteConfig s

-- | The notificationDaemon thread looks at the notification queue.
-- If the queue is empty and there is no current message, it sets the
-- new message as the current message in a TVar (Just Notification)
-- and displays the message itself and sets up a thread to remove the
-- message after its timeout.
--
-- If there is a current message, add the new message to the queue.
--
-- The timeout thread just sleeps for its timeout and then atomically
-- replaces the current message with the next one from the queue.  It
-- then displays the new current message.  However, if the current
-- message has changed (because of a user cancellation), the timer
-- thread just exits.
--
-- User cancellation atomically reads (and replaces) the current
-- notification (if there is another in the queue).  If it found a new
-- notification, that node is then displayed.
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
notify istate appName replaceId _ summary body _ _ timeout = do
  nid <- atomically $ do
    tid <- readTVar idsrc
    modifyTVar' idsrc (+1)
    return tid
  let realId = if replaceId == 0 then fromIntegral nid else replaceId
      n = Notification { noteAppName = appName
                       , noteReplaceId = replaceId
                       , noteSummary = escapeText summary
                       , noteBody = escapeText body
                       , noteExpireTimeout = tout
                       , noteId = realId
                       }
  -- If we are replacing an existing note, atomically do the swap in
  -- the note queue and then make this the new current if the queue is
  -- empty OR if the current has this id.
  dn <- atomically $ do
    modifyTVar' (noteQueue istate) (replaceNote n)
    cnote <- readTVar (noteCurrent istate)
    case cnote of
      Nothing -> do
        writeTVar (noteCurrent istate) (Just n)
        return (Just n)
      Just curNote
        | noteId curNote == realId -> do
          writeTVar (noteCurrent istate) (Just n)
          return (Just n)
        | otherwise -> do
          modifyTVar' (noteQueue istate) (|>n)
          return Nothing
  -- This is a little gross - if we added the new notification to the
  -- queue, we can't call displayNote on it because that will
  -- obliterate the current active notification.
  case dn of
    -- take no action; timeout threads will handle it
    Nothing -> return ()
    Just _ -> wakeupDisplayThread istate
  return realId
  where
    replaceNote newNote = fmap (\n -> if noteId n == noteReplaceId newNote then newNote else n)
    idsrc = noteIdSource istate
    escapeText = T.pack . escapeMarkup . T.unpack
    maxtout = fromIntegral $ notificationMaxTimeout (noteConfig istate)
    tout = case timeout of
      0 -> maxtout
      (-1) -> maxtout
      _ -> min maxtout timeout

notificationDaemon :: (AutoMethod f1, AutoMethod f2)
                      => f1 -> f2 -> IO ()
notificationDaemon onNote onCloseNote = do
  client <- connectSession
  _ <- requestName client "org.freedesktop.Notifications" [nameAllowReplacement, nameReplaceExisting]
  export client "/org/freedesktop/Notifications"
    [ autoMethod "org.freedesktop.Notifications" "GetServerInformation" getServerInformation
    , autoMethod "org.freedesktop.Notifications" "GetCapabilities" getCapabilities
    , autoMethod "org.freedesktop.Notifications" "CloseNotification" onCloseNote
    , autoMethod "org.freedesktop.Notifications" "Notify" onNote
    ]

-- | Wakeup the display thread and have it switch out the displayed
-- message for the new current message.
wakeupDisplayThread :: NotifyState -> IO ()
wakeupDisplayThread s = writeChan (noteChan s) ()

-- | This thread
displayThread :: NotifyState -> IO ()
displayThread s = forever $ do
  _ <- readChan (noteChan s)
  cur <- atomically $ readTVar (noteCurrent s)
  case cur of
    Nothing -> postGUIAsync (widgetHideAll (noteContainer s))
    Just n -> postGUIAsync $ do
      labelSetMarkup (noteWidget s) (formatMessage s n)
      widgetShowAll (noteContainer s)
      startTimeoutThread s n

startTimeoutThread :: NotifyState -> Notification -> IO ()
startTimeoutThread s n = do
  _ <- forkIO $ do
    let seconds = noteExpireTimeout n
    threadDelay (fromIntegral seconds * 1000000)
    atomically $ do
      curNote <- readTVar (noteCurrent s)
      case curNote of
        Nothing -> return ()
        Just cnote
          | cnote /= n -> return ()
          | otherwise ->
            -- The note was not invalidated or changed since the timeout
            -- began, so we replace it with the next (if any)
            nextNotification s
    wakeupDisplayThread s
  return ()

data NotificationConfig =
  NotificationConfig { notificationMaxTimeout :: Int -- ^ Maximum time that a notification will be displayed (in seconds).  Default: 10
                     , notificationMaxLength :: Int  -- ^ Maximum length displayed, in characters.  Default: 50
                     , notificationFormatter :: Notification -> String -- ^ Function used to format notifications
                     }

defaultFormatter :: Notification -> String
defaultFormatter note = msg
  where
    msg = case T.null (noteBody note) of
      True -> T.unpack $ noteSummary note
      False -> T.unpack $ mconcat [ "<span fgcolor='yellow'>Note:</span>"
                                  , noteSummary note, ": ", noteBody note ]

-- | The default formatter is one of
--
-- * Summary : Body
--
-- * Summary
--
-- depending on the presence of a notification body.
defaultNotificationConfig :: NotificationConfig
defaultNotificationConfig =
  NotificationConfig { notificationMaxTimeout = 10
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

  widgetHideAll frame

  istate <- initialNoteState (toWidget frame) textArea cfg
  _ <- on button buttonReleaseEvent (userCancel istate)

  realizableWrapper <- hBoxNew False 0
  boxPackStart realizableWrapper frame PackNatural 0
  widgetShow realizableWrapper

  -- We can't start the dbus listener thread until we are in the GTK
  -- main loop, otherwise things are prone to lock up and block
  -- infinitely on an mvar.  Bad stuff - only start the dbus thread
  -- after the fake invisible wrapper widget is realized.
  _ <- on realizableWrapper realize $ do
       _ <- forkIO (displayThread istate)
       notificationDaemon (notify istate) (closeNotification istate)

  -- Don't show the widget by default - it will appear when needed
  return (toWidget realizableWrapper)
  where
    -- | Close the current note and pull up the next, if any
    userCancel s = do
      liftIO $ do
        atomically $ nextNotification s
        wakeupDisplayThread s
      return True

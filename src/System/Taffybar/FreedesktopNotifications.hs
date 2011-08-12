{-# LANGUAGE OverloadedStrings #-}
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
import Data.Int ( Int32 )
import Data.IORef
import Data.Map ( Map )
import Data.Monoid ( mconcat )
import qualified Data.Sequence as S
import Data.Sequence ( Seq, (|>), viewl, ViewL(..) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Word ( Word32 )
import DBus.Client.Simple
import Graphics.UI.Gtk hiding ( Variant )
import Web.Encodings ( decodeHtml, encodeHtml )

-- | A simple structure representing a Freedesktop notification
data Notification = Notification { noteAppName :: Text
                                 , noteReplaceId :: Word32
                                 , noteSummary :: Text
                                 , noteBody :: Text
                                 , noteExpireTimeout :: Int32
                                 , noteId :: Word32
                                 }
                    deriving (Show, Eq)

data WorkType = CancelNote (Maybe Word32)
              | ReplaceNote Word32 Notification
              | NewNote
              | ExpireNote Word32

data NotifyState = NotifyState { noteQueue :: MVar (Seq Notification)
                               , noteIdSource :: MVar Word32
                               , noteWorkerChan :: Chan WorkType
                               , noteWidget :: Label
                               , noteContainer :: InfoBar
                               , noteTimerThread :: MVar (Maybe ThreadId)
                               , noteConfig :: NotificationConfig
                               }

initialNoteState :: InfoBar -> Label -> NotificationConfig -> IO NotifyState
initialNoteState ib l cfg = do
  c <- newChan
  m <- newMVar 1
  q <- newMVar S.empty
  t <- newMVar Nothing
  return NotifyState { noteQueue = q
                     , noteIdSource = m
                     , noteWorkerChan = c
                     , noteWidget = l
                     , noteContainer = ib
                     , noteTimerThread = t
                     , noteConfig = cfg
                     }

getServerInformation :: IO (Text, Text, Text, Text)
getServerInformation =
  return ("haskell-notification-daemon",
          "nochair.net",
          "0.0.1",
          "1.1")

getCapabilities :: IO [Text]
getCapabilities = return ["body", "body-markup"]

closeNotification :: NotifyState -> Word32 -> IO ()
closeNotification istate nid = do
  -- FIXME: filter anything with this nid out of the queue before
  -- posting to the queue so that the worker doesn't need to scan the
  -- queue
  writeChan (noteWorkerChan istate) (CancelNote (Just nid))

-- | Apply the user's formatter and truncate the result with the
-- specified maxlen.
formatMessage :: NotifyState -> Notification -> String
formatMessage s = take maxlen . fmt
  where
    maxlen = notificationMaxLength $ noteConfig s
    fmt = notificationFormatter $ noteConfig s

notify :: MVar Int
          -> NotifyState
          -> Text -- ^ Application name
          -> Word32 -- ^ Replaces id
          -> Text -- ^ App icon
          -> Text -- ^ Summary
          -> Text -- ^ Body
          -> [Text] -- ^ Actions
          -> Map Text Variant -- ^ Hints
          -> Int32 -- ^ Expires timeout (milliseconds)
          -> IO Word32
notify idSrc istate appName replaceId icon summary body actions hints timeout = do
  let maxtout = fromIntegral $ notificationMaxTimeout (noteConfig istate)
      tout = case timeout of
        0 -> maxtout
        (-1) -> maxtout
        _ -> min maxtout timeout
  case replaceId of
    0 -> do
      nid <- modifyMVar idSrc (\x -> return (x+1, x))
      let n = Notification { noteAppName = appName
                           , noteReplaceId = 0
                           , noteSummary = encodeHtml $ decodeHtml summary
                           , noteBody = encodeHtml $ decodeHtml body
                           , noteExpireTimeout = tout
                           , noteId = fromIntegral nid
                           }
      modifyMVar_ (noteQueue istate) (\x -> return (x |> n))
      writeChan (noteWorkerChan istate) NewNote
      return (fromIntegral nid)
    i -> do
      let n = Notification { noteAppName = appName
                           , noteReplaceId = i
                           , noteSummary = summary
                           , noteBody = body
                           , noteExpireTimeout = tout
                           , noteId = i
                           }
      -- First, replace any notes in the note queue with this note, if
      -- applicable.  Next, notify the worker and have it replace the
      -- current note if that note has this id.
      modifyMVar_ (noteQueue istate) (\q -> return $ fmap (replaceNote i n) q)
      writeChan (noteWorkerChan istate) (ReplaceNote i n)
      return i

replaceNote :: Word32 -> Notification -> Notification -> Notification
replaceNote nid newNote curNote =
  case noteId curNote == nid of
    False -> curNote
    True -> newNote

notificationDaemon onNote onCloseNote = do
  client <- connectSession
  _ <- requestName client "org.freedesktop.Notifications" [AllowReplacement, ReplaceExisting]
  export client "/org/freedesktop/Notifications"
    [ method "org.freedesktop.Notifications" "GetServerInformation" getServerInformation
    , method "org.freedesktop.Notifications" "GetCapabilities" getCapabilities
    , method "org.freedesktop.Notifications" "CloseNotification" onCloseNote
    , method "org.freedesktop.Notifications" "Notify" onNote
    ]

-- When a notification is received, add it to the queue.  Post a token to the channel that the
-- worker blocks on.

-- The worker thread should sit idle waiting on a chan read.  When it
-- wakes up, check to see if the current notification needs to be
-- expired (due to a cancellation) or just expired on its own.  If it
-- expired on its own, just empty it out and post the next item in the
-- queue, if any.  If posting, start a thread that just calls
-- theadDelay for the lifetime of the notification.

workerThread :: NotifyState -> IO ()
workerThread s = do
  currentNote <- newIORef Nothing
  workerThread' currentNote
  where
    workerThread' currentNote = do
      work <- readChan (noteWorkerChan s)
      case work of
        NewNote -> onNewNote currentNote
        ReplaceNote nid n -> onReplaceNote currentNote nid n
        CancelNote Nothing -> userCancelNote currentNote
        CancelNote nid -> do
          workerThread' currentNote
        ExpireNote nid -> expireNote currentNote nid
    -- | The user closed the notification manually
    userCancelNote currentNote = do
      writeIORef currentNote Nothing
      postGUIAsync $ widgetHideAll (noteContainer s)
      showNextNoteIfAny currentNote

    onReplaceNote currentNote nid n = do
      cnote <- readIORef currentNote
      case cnote of
        Nothing -> do
          writeIORef currentNote (Just n)
          postGUIAsync $ do
            labelSetMarkup (noteWidget s) (formatMessage s n)
            widgetShowAll (noteContainer s)
          timerThreadId <- forkIO $ setExpireTimeout (noteWorkerChan s) (noteId n) (noteExpireTimeout n)
          modifyMVar_ (noteTimerThread s) $ const (return (Just timerThreadId))
          workerThread' currentNote
        Just cnote' -> case noteId cnote' == nid of
          -- The replaced note was not current and it either does not
          -- exist or it was already replaced in the note queue
          False -> workerThread' currentNote
          -- Otherwise, swap out the current note
          True -> do
            withMVar (noteTimerThread s) (maybe (return ()) killThread)
            writeIORef currentNote (Just n)
            postGUIAsync $ labelSetMarkup (noteWidget s) (formatMessage s n)
            timerId <- forkIO $ setExpireTimeout (noteWorkerChan s) (noteId n) (noteExpireTimeout n)
            modifyMVar_ (noteTimerThread s) $ const $ return (Just timerId)
            workerThread' currentNote

    -- | If the current note has the ID being expired, clear the
    -- notification area and see if there is a pending note to post.
    expireNote currentNote nid = do
      cnote <- readIORef currentNote
      case cnote of
        Nothing -> showNextNoteIfAny currentNote
        Just cnote' ->
          case noteId cnote' == nid of
            False -> workerThread' currentNote -- Already expired
            True -> do
              -- Drop the reference and clear the notification area
              -- before trying to show a new note
              writeIORef currentNote Nothing
              postGUIAsync $ widgetHideAll (noteContainer s)
              showNextNoteIfAny currentNote

    onNewNote currentNote = do
      maybeCurrent <- readIORef currentNote
      case maybeCurrent of
        Nothing -> showNextNoteIfAny currentNote
          -- Grab the next note, show it, and then start a timer
        Just note -> do
          -- Otherwise, the current note isn't expired yet and we need
          -- to wait for it.
          workerThread' currentNote

    -- For use when there is no current note, attempt to show the next
    -- node and then block to wait for the next event.  This is
    -- guarded by a postGUIAsync.
    showNextNoteIfAny noCurrentNote = do
      nextNote <- modifyMVar (noteQueue s) takeNote
      case nextNote of
        Nothing -> workerThread' noCurrentNote
        Just nextNote' -> do
          writeIORef noCurrentNote nextNote
          postGUIAsync $ do
            labelSetMarkup (noteWidget s) (formatMessage s nextNote')
            widgetShowAll (noteContainer s)
          timerThreadId <- forkIO $ setExpireTimeout (noteWorkerChan s) (noteId nextNote') (noteExpireTimeout nextNote')
          modifyMVar_ (noteTimerThread s) $ const (return (Just timerThreadId))
          workerThread' noCurrentNote


takeNote :: Monad m => Seq a -> m (Seq a, Maybe a)
takeNote q =
  case viewl q of
    EmptyL -> return (q, Nothing)
    n :< rest -> return (rest, Just n)

setExpireTimeout :: Chan WorkType -> Word32 -> Int32 -> IO ()
setExpireTimeout c nid seconds = do
  threadDelay (fromIntegral seconds * 1000000)
  writeChan c (ExpireNote nid)

userCancel s _ = writeChan (noteWorkerChan s) (CancelNote Nothing)

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
      False -> T.unpack $ mconcat [ noteSummary note, ": ", noteBody note ]

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
                     , notificationMaxLength = 50
                     , notificationFormatter = defaultFormatter
                     }

-- | Create a new notification area with the given configuration.
notifyAreaNew :: NotificationConfig -> IO Widget
notifyAreaNew cfg = do
  ib <- infoBarNew
  l <- labelNew Nothing
  button <- buttonNew -- FromStock stockClose
  img <- imageNewFromStock stockClose (IconSizeUser 16)
  ca <- infoBarGetContentArea ib
  let container = castToContainer ca
  containerAdd container l
  containerAdd button img
  infoBarAddActionWidget ib button 0
  widgetHideAll ib



  istate <- initialNoteState ib l cfg
  _ <- on ib infoBarResponse (userCancel istate)
  _ <- forkIO (workerThread istate)

  -- This is only available to the notify handler, so it doesn't need
  -- to be protected from the worker thread.  There might be multiple
  -- notifiation handler threads, though (not sure), so keep it safe
  -- and use an mvar.
  idSrc <- newMVar 1
  notificationDaemon (notify idSrc istate) (closeNotification istate)

  -- Don't show ib by default - it will appear when needed
  return (toWidget ib)
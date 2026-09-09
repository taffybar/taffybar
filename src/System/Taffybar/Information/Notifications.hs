-- | Notification storage with replacement-aware expiration.
module System.Taffybar.Information.Notifications
  ( Notification (..),
    NotificationQueue,
    newNotificationQueue,
    notificationUpdates,
    readNotifications,
    enqueueNotification,
    removeNotification,
    nextNotification,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (mask_)
import Control.Monad (forM_, void, when)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import Data.Word (Word32)

data Notification = Notification
  { noteAppName :: Text,
    noteReplaceId :: Word32,
    noteSummary :: Text,
    noteBody :: Text,
    noteExpireTimeout :: Maybe Int32,
    noteId :: Word32
  }
  deriving (Show, Eq)

data NotificationQueue = NotificationQueue
  { queuedNotifications :: TVar (Seq (Unique, Notification)),
    notificationUpdates :: TChan ()
  }

newNotificationQueue :: IO NotificationQueue
newNotificationQueue =
  NotificationQueue <$> newTVarIO Seq.empty <*> newBroadcastTChanIO

readNotifications :: NotificationQueue -> IO [Notification]
readNotifications queue = map snd . toList <$> readTVarIO (queuedNotifications queue)

enqueueNotification :: NotificationQueue -> Notification -> IO ()
enqueueNotification queue notification = mask_ $ do
  generation <- newUnique
  atomically $ do
    current <- readTVar $ queuedNotifications queue
    let entry = (generation, notification)
        updated = case Seq.findIndexL ((== noteId notification) . noteId . snd) current of
          Nothing -> current Seq.|> entry
          Just index -> Seq.update index entry current
    writeTVar (queuedNotifications queue) updated
    writeTChan (notificationUpdates queue) ()
  forM_ (noteExpireTimeout notification) $ \milliseconds -> void $ forkIO $ do
    elapsed <- registerDelay (max 0 (fromIntegral milliseconds) * 1000)
    atomically $ do
      current <- readTVar $ queuedNotifications queue
      when (any ((== generation) . fst) current) $ do
        readTVar elapsed >>= check
        writeTVar (queuedNotifications queue) $ Seq.filter ((/= generation) . fst) current
        writeTChan (notificationUpdates queue) ()

removeNotification :: NotificationQueue -> Word32 -> IO ()
removeNotification queue identifier = atomically $ do
  modifyTVar' (queuedNotifications queue) $ Seq.filter ((/= identifier) . noteId . snd)
  writeTChan (notificationUpdates queue) ()

nextNotification :: NotificationQueue -> IO ()
nextNotification queue = atomically $ do
  modifyTVar' (queuedNotifications queue) (Seq.drop 1)
  writeTChan (notificationUpdates queue) ()

{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Information.NotificationsSpec (spec) where

import Control.Concurrent (threadDelay)
import System.Taffybar.Information.Notifications
import System.Timeout (timeout)
import Test.Hspec

notification :: Notification
notification = Notification "test" 0 "message" "" (Just 40) 1

spec :: Spec
spec = describe "Notification expiration" $ do
  it "does not let the original deadline remove a replacement" $ do
    queue <- newNotificationQueue
    enqueueNotification queue notification
    let replacement = notification {noteExpireTimeout = Nothing}
    enqueueNotification queue replacement
    threadDelay 100000
    readNotifications queue `shouldReturn` [replacement]

  it "expires a replacement at its own deadline" $ do
    queue <- newNotificationQueue
    enqueueNotification queue notification {noteExpireTimeout = Nothing}
    enqueueNotification queue notification
    timeout 1000000 (waitUntilEmpty queue) `shouldReturn` Just ()

  it "does not expire a reused ID after closing its original notification" $ do
    queue <- newNotificationQueue
    enqueueNotification queue notification
    removeNotification queue 1
    let replacement = notification {noteExpireTimeout = Nothing}
    enqueueNotification queue replacement
    threadDelay 100000
    readNotifications queue `shouldReturn` [replacement]

waitUntilEmpty :: NotificationQueue -> IO ()
waitUntilEmpty queue = do
  entries <- readNotifications queue
  if null entries then pure () else threadDelay 1000 >> waitUntilEmpty queue

{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Privacy
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Privacy indicator widget for taffybar.
--
-- Shows icons when microphone, camera, or screen sharing is active.
-- The widget is hidden when no privacy-relevant streams are active.
--
-- Example usage:
--
-- @
-- import System.Taffybar.Widget.Privacy
--
-- myConfig = defaultSimpleTaffyConfig
--   { endWidgets = [ privacyNew ]
--   }
-- @
--
-- CSS classes:
--
-- * @.privacy-widget@ - The main container
-- * @.privacy-audio-input@ - Audio input (microphone) icon
-- * @.privacy-audio-output@ - Audio output icon
-- * @.privacy-video-input@ - Video input (camera/screen share) icon
module System.Taffybar.Widget.Privacy
  ( -- * Widget constructors
    privacyNew,
    privacyNewWith,

    -- * Configuration
    PrivacyWidgetConfig (..),
    defaultPrivacyWidgetConfig,

    -- * Re-exports
    PrivacyConfig (..),
    defaultPrivacyConfig,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Default (Default (..))
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Privacy
  ( NodeType (..),
    PrivacyConfig (..),
    PrivacyInfo (..),
    PrivacyNode (..),
    defaultPrivacyConfig,
    getPrivacyInfoChan,
    getPrivacyInfoState,
  )
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Util (widgetSetClassGI)

-- | Configuration for the privacy widget.
data PrivacyWidgetConfig = PrivacyWidgetConfig
  { -- | Underlying privacy monitoring configuration
    privacyWidgetConfig :: PrivacyConfig,
    -- | Icon name for microphone
    audioInputIcon :: T.Text,
    -- | Icon name for audio output
    audioOutputIcon :: T.Text,
    -- | Icon name for camera/screen share
    videoInputIcon :: T.Text,
    -- | Size of the icons (as Int32 for GTK)
    privacyIconSize :: Int32,
    -- | Spacing between icons
    privacySpacing :: Int
  }
  deriving (Eq, Show)

-- | Default privacy widget configuration.
defaultPrivacyWidgetConfig :: PrivacyWidgetConfig
defaultPrivacyWidgetConfig =
  PrivacyWidgetConfig
    { privacyWidgetConfig = defaultPrivacyConfig,
      audioInputIcon = "audio-input-microphone-symbolic",
      audioOutputIcon = "audio-speakers-symbolic",
      videoInputIcon = "camera-video-symbolic",
      privacyIconSize = fromIntegral $ fromEnum Gtk.IconSizeMenu,
      privacySpacing = 4
    }

instance Default PrivacyWidgetConfig where
  def = defaultPrivacyWidgetConfig

-- | Create a privacy indicator widget with default configuration.
privacyNew :: TaffyIO Gtk.Widget
privacyNew = privacyNewWith defaultPrivacyWidgetConfig

-- | Create a privacy indicator widget with custom configuration.
privacyNewWith :: PrivacyWidgetConfig -> TaffyIO Gtk.Widget
privacyNewWith config = do
  chan <- getPrivacyInfoChan (privacyWidgetConfig config)
  initialInfo <- getPrivacyInfoState (privacyWidgetConfig config)

  liftIO $ do
    -- Create main container box
    box <- Gtk.boxNew Gtk.OrientationHorizontal (fromIntegral $ privacySpacing config)
    _ <- widgetSetClassGI box "privacy-widget"

    -- Create icons for each type (initially hidden)
    audioInImage <- createIcon (audioInputIcon config) (privacyIconSize config)
    audioOutImage <- createIcon (audioOutputIcon config) (privacyIconSize config)
    videoInImage <- createIcon (videoInputIcon config) (privacyIconSize config)

    _ <- widgetSetClassGI audioInImage "privacy-audio-input"
    _ <- widgetSetClassGI audioOutImage "privacy-audio-output"
    _ <- widgetSetClassGI videoInImage "privacy-video-input"

    -- Add icons to box
    Gtk.containerAdd box audioInImage
    Gtk.containerAdd box audioOutImage
    Gtk.containerAdd box videoInImage

    -- Create a revealer to control visibility with animation
    revealer <- Gtk.revealerNew
    Gtk.revealerSetTransitionType revealer Gtk.RevealerTransitionTypeSlideLeft
    Gtk.revealerSetTransitionDuration revealer 200
    Gtk.containerAdd revealer box

    -- Update function
    let updateWidget info = postGUIASync $ do
          let nodes = activeNodes info
              hasAudioIn = any ((== AudioInput) . nodeType) nodes
              hasAudioOut = any ((== AudioOutput) . nodeType) nodes
              hasVideoIn = any ((== VideoInput) . nodeType) nodes
              hasAny = hasAudioIn || hasAudioOut || hasVideoIn

          -- Show/hide individual icons
          Gtk.widgetSetVisible audioInImage hasAudioIn
          Gtk.widgetSetVisible audioOutImage hasAudioOut
          Gtk.widgetSetVisible videoInImage hasVideoIn

          -- Show/hide the whole widget
          Gtk.revealerSetRevealChild revealer hasAny

          -- Update tooltip
          when hasAny $ do
            let tooltipText = buildTooltip nodes
            Gtk.widgetSetTooltipText box (Just $ T.pack tooltipText)

    -- Initial update
    updateWidget initialInfo

    -- Connect to channel updates
    void $ Gtk.onWidgetRealize revealer $ do
      ourChan <- atomically $ dupTChan chan
      void $ Gtk.onWidgetUnrealize revealer $ return ()
      -- Start update thread
      let loop = do
            info <- atomically $ readTChan ourChan
            updateWidget info
            loop
      _ <- forkIO loop
      return ()

    Gtk.widgetShowAll revealer
    Gtk.toWidget revealer

-- | Create an icon image.
createIcon :: T.Text -> Int32 -> IO Gtk.Image
createIcon iconName size = do
  image <- Gtk.imageNewFromIconName (Just iconName) size
  Gtk.widgetSetVisible image False
  return image

-- | Build tooltip text from active nodes.
buildTooltip :: [PrivacyNode] -> String
buildTooltip nodes =
  let audioInNodes = filter ((== AudioInput) . nodeType) nodes
      audioOutNodes = filter ((== AudioOutput) . nodeType) nodes
      videoInNodes = filter ((== VideoInput) . nodeType) nodes

      formatSection :: String -> [PrivacyNode] -> Maybe String
      formatSection title ns
        | null ns = Nothing
        | otherwise = Just $ title ++ ":\n" ++ unlines (map formatNode ns)

      formatNode n = "  - " ++ T.unpack (appName n)

      sections =
        catMaybes
          [ formatSection "Microphone" audioInNodes,
            formatSection "Camera/Screen" videoInNodes,
            formatSection "Audio Output" audioOutNodes
          ]
   in intercalate "\n" sections

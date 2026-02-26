{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Graphics.UI.EWMHStrut where

import           Control.Monad.IO.Class
import           Data.Int
import           Data.Text
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import qualified GI.Gdk as Gdk

data EWMHStrutSettings = EWMHStrutSettings
  { _left :: Int32
  , _right :: Int32
  , _top :: Int32
  , _bottom :: Int32
  , _left_start_y :: Int32
  , _left_end_y :: Int32
  , _right_start_y :: Int32
  , _right_end_y :: Int32
  , _top_start_x :: Int32
  , _top_end_x :: Int32
  , _bottom_start_x :: Int32
  , _bottom_end_x :: Int32
  } deriving (Show, Eq)

zeroStrutSettings = EWMHStrutSettings
  { _left = 0
  , _right = 0
  , _top = 0
  , _bottom = 0
  , _left_start_y = 0
  , _left_end_y = 0
  , _right_start_y = 0
  , _right_end_y = 0
  , _top_start_x = 0
  , _top_end_x = 0
  , _bottom_start_x = 0
  , _bottom_end_x = 0
  }

scaleStrutSettings :: Int32 -> EWMHStrutSettings -> EWMHStrutSettings
scaleStrutSettings scaleFactor st = st
  { _left = _left st * scaleFactor
  , _right = _right st * scaleFactor
  , _top = _top st * scaleFactor
  , _bottom = _bottom st * scaleFactor
  , _left_start_y = _left_start_y st * scaleFactor
  , _left_end_y = _left_end_y st * scaleFactor
  , _right_start_y = _right_start_y st * scaleFactor
  , _right_end_y = _right_end_y st * scaleFactor
  , _top_start_x = _top_start_x st * scaleFactor
  , _top_end_x = _top_end_x st * scaleFactor
  , _bottom_start_x = _bottom_start_x st * scaleFactor
  , _bottom_end_x = _bottom_end_x st * scaleFactor
  }

strutSettingsToPtr :: MonadIO m => EWMHStrutSettings -> m (Ptr CULong)
strutSettingsToPtr EWMHStrutSettings
                     { _left = left
                     , _right = right
                     , _top = top
                     , _bottom = bottom
                     , _left_start_y = left_start_y
                     , _left_end_y = left_end_y
                     , _right_start_y = right_start_y
                     , _right_end_y = right_end_y
                     , _top_start_x = top_start_x
                     , _top_end_x = top_end_x
                     , _bottom_start_x = bottom_start_x
                     , _bottom_end_x = bottom_end_x
                     } = liftIO $ do
  arr <- mallocArray 12
  let doPoke off v = pokeElemOff arr off $ fromIntegral v
  doPoke 0 left
  doPoke 1 right
  doPoke 2 top
  doPoke 3 bottom
  doPoke 4 left_start_y
  doPoke 5 left_end_y
  doPoke 6 right_start_y
  doPoke 7 right_end_y
  doPoke 8 top_start_x
  doPoke 9 top_end_x
  doPoke 10 bottom_start_x
  doPoke 11 bottom_end_x
  return arr

foreign import ccall "gdk_property_change" gdk_property_change ::
  Ptr Gdk.Window ->
    Ptr Gdk.Atom -> Ptr Gdk.Atom -> Int32 -> CUInt -> Ptr CUChar -> Int32 -> IO ()

propertyChange
  :: (Gdk.IsWindow a, MonadIO m)
  => a
  -> Gdk.Atom
  -> Gdk.Atom
  -> Int32
  -> Gdk.PropMode
  -> Ptr CUChar
  -> Int32
  -> m ()
propertyChange window property type_ format mode data_ nelements = liftIO $ do
    window' <- Gdk.unsafeManagedPtrCastPtr window
    property' <- Gdk.unsafeManagedPtrGetPtr property
    type_' <- Gdk.unsafeManagedPtrGetPtr type_
    let mode' = (fromIntegral . fromEnum) mode
    gdk_property_change window' property' type_' format mode' data_ nelements
    Gdk.touchManagedPtr window
    Gdk.touchManagedPtr property
    Gdk.touchManagedPtr type_
    return ()

setStrut :: MonadIO m => Gdk.IsWindow w => w -> EWMHStrutSettings -> m ()
setStrut w settings = do
  strutAtom <- Gdk.atomIntern "_NET_WM_STRUT_PARTIAL" False
  cardinalAtom <- Gdk.atomIntern "CARDINAL" False
  settingsArray <- castPtr <$> strutSettingsToPtr settings
  propertyChange w strutAtom cardinalAtom 32 Gdk.PropModeReplace settingsArray 12

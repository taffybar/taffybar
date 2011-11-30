module System.Taffybar.StrutProperties ( setStrutProperties
                                       , StrutProperties ) where

import Graphics.UI.Gtk

import Foreign
import Foreign.C.Types
import Unsafe.Coerce ( unsafeCoerce )

type StrutProperties = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

foreign import ccall "set_strut_properties"
    c_set_strut_properties :: Ptr Window -> CLong -> CLong -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> ()

-- | Reserve EWMH struts
setStrutProperties :: Window -> StrutProperties -> IO ()
setStrutProperties gtkWindow (left, right, top, bottom,
                                left_start_y, left_end_y,
                                right_start_y, right_end_y,
                                top_start_x, top_end_x,
                                bottom_start_x, bottom_end_x) = do
    let ptrWin = unsafeCoerce gtkWindow :: ForeignPtr Window
    let fi = fromIntegral
    withForeignPtr ptrWin $ \realPointer -> do
        return $ c_set_strut_properties realPointer (fi left) (fi right) (fi top) (fi bottom)
                                                        (fi left_start_y) (fi left_end_y)
                                                        (fi right_start_y) (fi right_end_y)
                                                        (fi top_start_x) (fi top_end_x)
                                                        (fi bottom_start_x) (fi bottom_end_x)


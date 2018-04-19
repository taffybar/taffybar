{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.PixbufCompat where

import qualified Data.GI.Base.CallStack as B.CallStack
import           Data.GI.Base.ShortPrelude
import qualified GI.GdkPixbuf.Callbacks as GdkPixbuf.Callbacks
import qualified GI.GdkPixbuf.Enums as GdkPixbuf.Enums
import           GI.GdkPixbuf.Objects.Pixbuf

-- TODO: Remove this once a better approach is found


foreign import ccall "gdk_pixbuf_new_from_data" gdk_pixbuf_new_from_data ::
    Ptr Word8 ->                            -- data : TCArray False (-1) (-1) (TBasicType TUInt8)
    CUInt ->                                -- colorspace : TInterface (Name {namespace = "GdkPixbuf", name = "Colorspace"})
    CInt ->                                 -- has_alpha : TBasicType TBoolean
    Int32 ->                                -- bits_per_sample : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    Int32 ->                                -- rowstride : TBasicType TInt
    FunPtr GdkPixbuf.Callbacks.C_PixbufDestroyNotify -> -- destroy_fn : TInterface (Name {namespace = "GdkPixbuf", name = "PixbufDestroyNotify"})
    Ptr () ->                               -- destroy_fn_data : TBasicType TPtr
    IO (Ptr Pixbuf)

{- |
Creates a new 'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' out of in-memory image data.  Currently only RGB
images with 8 bits per sample are supported.

Since you are providing a pre-allocated pixel buffer, you must also
specify a way to free that data.  This is done with a function of
type 'GI.GdkPixbuf.Callbacks.PixbufDestroyNotify'.  When a pixbuf created with is
finalized, your destroy notification function will be called, and
it is its responsibility to free the pixel array.

See also 'GI.GdkPixbuf.Objects.Pixbuf.pixbufNewFromBytes'.
-}
pixbufNewFromData' ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Ptr Word8
    {- ^ /@data@/: Image data in 8-bit\/sample packed format -}
    -> GdkPixbuf.Enums.Colorspace
    {- ^ /@colorspace@/: Colorspace for the image data -}
    -> Bool
    {- ^ /@hasAlpha@/: Whether the data has an opacity channel -}
    -> Int32
    {- ^ /@bitsPerSample@/: Number of bits per sample -}
    -> Int32
    {- ^ /@width@/: Width of the image in pixels, must be > 0 -}
    -> Int32
    {- ^ /@height@/: Height of the image in pixels, must be > 0 -}
    -> Int32
    {- ^ /@rowstride@/: Distance in bytes between row starts -}
    -> Maybe GdkPixbuf.Callbacks.PixbufDestroyNotify
    {- ^ /@destroyFn@/: Function used to free the data when the pixbuf\'s reference count
drops to zero, or 'Nothing' if the data should not be freed -}
    -> m Pixbuf
    {- ^ __Returns:__ A newly-created 'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' structure with a reference count of 1. -}
pixbufNewFromData' data_ colorspace hasAlpha bitsPerSample width height rowstride destroyFn = liftIO $ do
    let colorspace' = (fromIntegral . fromEnum) colorspace
    let hasAlpha' = (fromIntegral . fromEnum) hasAlpha
    ptrdestroyFn <- callocMem :: IO (Ptr (FunPtr GdkPixbuf.Callbacks.C_PixbufDestroyNotify))
    maybeDestroyFn <- case destroyFn of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jDestroyFn -> do
            jDestroyFn' <- GdkPixbuf.Callbacks.mk_PixbufDestroyNotify (GdkPixbuf.Callbacks.wrap_PixbufDestroyNotify (Just ptrdestroyFn) (GdkPixbuf.Callbacks.drop_closures_PixbufDestroyNotify jDestroyFn))
            poke ptrdestroyFn jDestroyFn'
            return jDestroyFn'
    let destroyFnData = nullPtr
    result <- gdk_pixbuf_new_from_data data_ colorspace' hasAlpha' bitsPerSample width height rowstride maybeDestroyFn destroyFnData
    checkUnexpectedReturnNULL "pixbufNewFromData" result
    wrapObject Pixbuf result

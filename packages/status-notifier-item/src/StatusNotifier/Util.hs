{-# LANGUAGE OverloadedStrings #-}

module StatusNotifier.Util where

import Control.Arrow
import Control.Lens
import DBus (methodError)
import DBus.Client
import qualified DBus.Generation as G
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.ByteString
import Data.Word
import Language.Haskell.TH
import StatusNotifier.TH
import System.ByteOrder (fromBigEndian)
import System.Log.Logger
import System.Timeout (timeout)

-- | Run a DBus call with an upper bound on how long it may block. An
-- unresponsive peer (a process that owns a bus name but never services its
-- connection) otherwise blocks callers indefinitely, since the dbus library
-- does not time out calls on its own. On timeout, the call resolves to a
-- 'M.MethodError' so existing error handling drops the peer gracefully.
callWithTimeout ::
  Int ->
  IO (Either M.MethodError a) ->
  IO (Either M.MethodError a)
callWithTimeout micros action =
  fromMaybe (Left timeoutError) <$> timeout micros action
  where
    timeoutError = methodError (T.Serial 0) errorFailed

splitServiceName :: String -> (String, Maybe String)
splitServiceName name =
  case break (== '/') name of
    (bus, "") -> (bus, Nothing)
    (bus, path) | not (null bus) -> (bus, Just path)
    _ -> (name, Nothing)

getIntrospectionObjectFromFile :: FilePath -> T.ObjectPath -> Q I.Object
getIntrospectionObjectFromFile filepath nodePath = do
  object <- runIO $ I.parseXML nodePath <$> TIO.readFile filepath
  maybe
    (fail $ "Unable to parse introspection XML from " <> filepath)
    pure
    object

generateClientFromFile :: G.GenerationParams -> Bool -> FilePath -> Q [Dec]
generateClientFromFile params useObjectPath filepath = do
  object <- getIntrospectionObjectFromFile filepath "/"
  interface <-
    case I.objectInterfaces object of
      interface : _ -> pure interface
      [] -> fail $ "No interfaces found in introspection XML from " <> filepath
  let actualObjectPath = I.objectPath object
      realParams =
        if useObjectPath
          then params {G.genObjectPath = Just actualObjectPath}
          else params
  (++)
    <$> G.generateClient realParams interface
    <*> G.generateSignalsFromInterface realParams interface

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

makeLensesWithLSuffix :: Name -> DecsQ
makeLensesWithLSuffix =
  makeLensesWith $
    lensRules
      & lensField .~ \_ _ name ->
        [TopName (mkName $ nameBase name ++ "L")]

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe $ return ()

convertARGBToABGR :: Word32 -> Word32
convertARGBToABGR bits = (blue `shift` 16) .|. (red `shift` (-16)) .|. green .|. alpha
  where
    blue = bits .&. 0xFF
    green = bits .&. 0xFF00
    red = bits .&. 0xFF0000
    alpha = bits .&. 0xFF000000

networkToSystemByteOrder :: BS.ByteString -> BS.ByteString
networkToSystemByteOrder original =
  vectorToByteString $ VS.map (convertARGBToABGR . fromBigEndian) $ byteStringToVector original

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

makeErrorReply :: ErrorName -> String -> Reply
makeErrorReply e message = ReplyError e [T.toVariant message]

logErrorWithDefault ::
  (Show a) => (Priority -> String -> IO ()) -> b -> String -> Either a b -> IO b
logErrorWithDefault logger def message =
  fmap (fromMaybe def) . logEitherError logger message

logEitherError :: (Show a) => (Priority -> String -> IO ()) -> String -> Either a b -> IO (Maybe b)
logEitherError logger message =
  either (\err -> logger ERROR (message ++ show err) >> return Nothing) (return . Just)

exemptUnknownMethod ::
  b -> Either M.MethodError b -> Either M.MethodError b
exemptUnknownMethod def eitherV =
  case eitherV of
    Right _ -> eitherV
    Left M.MethodError {M.methodErrorName = errorName} ->
      if errorName == errorUnknownMethod
        then Right def
        else eitherV

exemptAll ::
  b -> Either M.MethodError b -> Either M.MethodError b
exemptAll def eitherV =
  case eitherV of
    Right _ -> eitherV
    Left _ -> Right def

infixl 4 <..>

(<..>) :: (Functor f) => (a -> b) -> f (f a) -> f (f b)
(<..>) = fmap . fmap

infixl 4 <<$>>

(<<$>>) :: (a -> IO b) -> Maybe a -> IO (Maybe b)
fn <<$>> m = traverse fn m

forkM :: (Monad m) => (i -> m a) -> (i -> m b) -> i -> m (a, b)
forkM a b i =
  do
    r1 <- a i
    r2 <- b i
    return (r1, r2)

tee :: (Monad m) => (i -> m a) -> (i -> m b) -> i -> m a
tee = (fmap . fmap . fmap) (fmap fst) forkM

(>>=/) :: (Monad m) => m a -> (a -> m b) -> m a
(>>=/) a = (a >>=) . tee return

getInterfaceAt ::
  Client ->
  T.BusName ->
  T.ObjectPath ->
  IO (Either M.MethodError (Maybe I.Object))
getInterfaceAt client bus path =
  right (I.parseXML "/" . pack) <$> introspect client bus path

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM p =
  foldr (\x -> ifM (p x) (return $ Just x)) (return Nothing)

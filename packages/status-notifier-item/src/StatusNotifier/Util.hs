{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Util where

import           Control.Arrow
import           Control.Lens
import           DBus.Client
import qualified DBus.Generation as G
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Maybe
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable.ByteString
import           Data.Word
import           Language.Haskell.TH
import           StatusNotifier.TH
import qualified Data.Text.IO as TIO
import           Data.Text (pack)
import           System.ByteOrder (fromBigEndian)
import           System.Log.Logger

splitServiceName :: String -> (String, Maybe String)
splitServiceName name =
  case break (=='/') name of
    (bus, "") -> (bus, Nothing)
    (bus, path) | not (null bus) -> (bus, Just path)
    _ -> (name, Nothing)

getIntrospectionObjectFromFile :: FilePath -> T.ObjectPath -> Q I.Object
getIntrospectionObjectFromFile filepath nodePath = runIO $
  head . maybeToList . I.parseXML nodePath <$> TIO.readFile filepath

generateClientFromFile :: G.GenerationParams -> Bool -> FilePath -> Q [Dec]
generateClientFromFile params useObjectPath filepath = do
  object <- getIntrospectionObjectFromFile filepath "/"
  let interface = head $ I.objectInterfaces object
      actualObjectPath = I.objectPath object
      realParams =
        if useObjectPath
        then params { G.genObjectPath = Just actualObjectPath }
        else params
  (++) <$> G.generateClient realParams interface <*>
           G.generateSignalsFromInterface realParams interface

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

makeLensesWithLSuffix :: Name -> DecsQ
makeLensesWithLSuffix =
  makeLensesWith $
  lensRules & lensField .~ \_ _ name ->
    [TopName (mkName $ nameBase name ++ "L")]

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
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
  Show a => (Priority -> String -> IO ()) -> b -> String -> Either a b -> IO b
logErrorWithDefault logger def message =
  fmap (fromMaybe def) . logEitherError logger message

logEitherError :: Show a => (Priority -> String -> IO ()) -> String -> Either a b -> IO (Maybe b)
logEitherError logger message =
  either (\err -> logger ERROR (message ++ show err) >> return Nothing) (return . Just)

exemptUnknownMethod ::
  b -> Either M.MethodError b -> Either M.MethodError b
exemptUnknownMethod def eitherV =
  case eitherV of
    Right _ -> eitherV
    Left M.MethodError { M.methodErrorName = errorName } ->
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
(<..>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
(<..>) = fmap . fmap

infixl 4 <<$>>
(<<$>>) :: (a -> IO b) -> Maybe a -> IO (Maybe b)
fn <<$>> m = sequenceA $ fn <$> m

forkM :: Monad m => (i -> m a) -> (i -> m b) -> i -> m (a, b)
forkM a b i =
  do
    r1 <- a i
    r2 <- b i
    return (r1, r2)

tee :: Monad m => (i -> m a) -> (i -> m b) -> i -> m a
tee = (fmap . fmap . fmap) (fmap fst) forkM

(>>=/) :: Monad m => m a -> (a -> m b) -> m a
(>>=/) a = (a >>=) . tee return

getInterfaceAt
  :: Client
  -> T.BusName
  -> T.ObjectPath
  -> IO (Either M.MethodError (Maybe I.Object))
getInterfaceAt client bus path =
  right (I.parseXML "/" . pack) <$> introspect client bus path

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)

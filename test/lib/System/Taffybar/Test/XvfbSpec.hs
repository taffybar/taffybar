{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Taffybar.Test.XvfbSpec
  ( spec
  -- * Virtual X11 server for unit testing
  , withXvfb
  , withXdummy
  , displayArg
  , displayEnv
  , setDefaultDisplay_
  , setDefaultDisplay
  -- ** Randr
  , withRandrSetup
  , randrSetup
  , randrTeardown
  -- *** RANDR config
  , RRSetup(..)
  , RROutput(..)
  , RROutputSettings(..)
  , RRExistingMode(..)
  , RRMode(..)
  , RRModeName(..)
  , RRModeLine(..)
  , RRPosition(..)
  , RRRotation(..)
  , ListIndex(..)
  -- ** Clients
  , withXTerm
  -- * Wrappers around @xprop@ command
  , XPropName(..)
  , xpropName
  , XPropValue(..)
  , xpropValue
  , xpropGet
  , xpropSet
  , xpropRemove
  , xpropList
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), void, forM_, guard)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Attoparsec.Text.Lazy hiding (takeWhile, take)
import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bifunctor (bimap, second)
import Data.Char (isPrint, isSpace)
import Data.Coerce (coerce)
import Data.Default (Default(..))
import Data.List (findIndex, uncons, dropWhileEnd)
import Data.String (IsString(..))
import Data.Text.Lazy.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Function ((&))
import Data.Maybe (maybeToList, mapMaybe, isJust, isNothing)
import GHC.Generics (Generic)
import System.Process.Typed
import System.IO (Handle, hClose, hGetLine)
import Text.Read (readMaybe)
import UnliftIO.Async (race)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (fromEitherM, throwString, bracket_, throwIO)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Taffybar.Test.UtilSpec (withSetEnv, logSetup, specLog, specLogAt, getSpecLogPriority, Priority(..))
import System.Taffybar.Information.X11DesktopInfo (DisplayName(..))

------------------------------------------------------------------------

-- | Construct a 'DisplayName' for the given local display number.
displayNumber :: Int -> DisplayName
displayNumber n = DisplayName (displaySpec n)

displaySpec :: Int -> String
displaySpec n = ":" ++ show n

-- | Produce a @-display@ command-line option, supported by many X11
-- programs.
displayArg :: DisplayName -> [String]
displayArg DefaultDisplay = []
displayArg (DisplayName d) = ["-display", d]

displayEnv :: DisplayName -> [(String, String)]
displayEnv DefaultDisplay = []
displayEnv (DisplayName d) = [("DISPLAY", d)]

------------------------------------------------------------------------

newtype XPropName = XPropName { unXPropName :: String }
  deriving (Show, Read, Eq, Ord, Generic)

-- | Construct a valid 'XPropName' from a 'String'. Any names which
-- cause problems when parsing @xprop@ output are not allowed.
xpropName :: String -> Maybe XPropName
xpropName n@(c:cs)
  | c `elem` propNameStartChars &&
    all isPrint cs &&
    not (any propNameBadChars cs) = Just (XPropName n)
xpropName _ = Nothing

-- | List of characters which are valid first characters of a property
-- name.
propNameStartChars :: [Char]
propNameStartChars = ['a'..'z'] <> ['A'..'Z'] <> ['_']

propNameBadChars :: Char -> Bool
propNameBadChars c = isSpace c || c `elem` ['(', ')', ':']

newtype XPropValue = XPropValue { unXPropValue :: String }
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid, Generic)

-- | Construct a valid 'XPropValue' from a 'String'. Any values which
-- cause problems when parsing @xprop@ output are not allowed.
xpropValue :: String -> Maybe XPropValue
xpropValue n = if not (any propValueBadChars n) then Just (XPropValue n) else Nothing

-- | Predicate on characters which cause difficulties when parsing
-- @xprop@ output.
propValueBadChars :: Char -> Bool
propValueBadChars c = c `elem` ['"','\n','\r'] || not (isPrint c)

------------------------------------------------------------------------

xpropProc :: DisplayName -> [String] -> ProcessConfig () () ()
xpropProc d args = proc "xprop" (displayArg d ++ args)

xpropGet :: (HasCallStack, MonadIO m) => DisplayName -> XPropName -> m [XPropValue]
xpropGet d p = do
  specLog $ "xpropGet running: " ++ show cfg
  txt <- decoded $ readProcessStdout_ cfg
  specLogAt DEBUG $ "xprop output:\n" ++ TL.unpack txt
  either throwString pure $ parseXProp1 p txt
  where
    cfg = xpropProc d ["-root", unXPropName p]
    decoded :: MonadIO m => m BL.ByteString -> m TL.Text
    decoded = fromEitherM . fmap decodeUtf8'

parseXProp1 :: XPropName -> TL.Text -> Either String [XPropValue]
parseXProp1 p = maybe (Left ("property " ++ show p ++ " not found")) Right . lookup p <=< parseXProp

parseXProp :: TL.Text -> Either String [(XPropName, [XPropValue])]
parseXProp = eitherResult . parse (many' parseProp)
  where
    parseProp = do
      n <- XPropName . T.unpack <$> parsePropName
      vs <- parsePropValues <|> parseErrorMessage
      pure (n, vs)
    parseErrorMessage = do
      void $ char ':'
      skipWhile isHorizontalSpace
      msg <- takeTill isEndOfLine
      fail $ T.unpack msg
    parsePropValues = do
      t <- parsePropType
      skipWhile isHorizontalSpace
      void $ char '='
      skipWhile isHorizontalSpace
      vs <- parsePropValue t `sepBy` parseValueSep
      endOfLine
      pure $ map (XPropValue . T.unpack) vs
    parsePropName = P.takeWhile (\c -> c /= '(' && c /= ':')
    parsePropType = char '(' *> P.takeWhile (/= ')') <* char ')'
    parseValueSep = char ',' <* skipWhile isHorizontalSpace
    takeUntilSep = takeTill (\c -> c == ',' || isEndOfLine c)
    quotedString = quotedString' >>= unescape
    quotedString' = char '"' *> P.takeWhile (/= '"') <* char '"'
    unescape = pure . T.replace "\\\"" "\"" . T.replace "\\\\" "\\"
    parsePropValue "CARDINAL" = takeUntilSep
    parsePropValue "ATOM" = takeUntilSep
    parsePropValue "STRING" = quotedString
    parsePropValue "UTF8_STRING" = quotedString
    parsePropValue t = fail $ "Can't parse format \"" ++ T.unpack t ++ "\""

xpropSet :: MonadIO m => DisplayName -> XPropName -> XPropValue -> m ()
xpropSet d (XPropName p) (XPropValue v) = liftIO $ do
  specLog $ "xpropSet running: " ++ show cfg
  runProcess_ cfg
  where
    cfg = xpropProc d args
    args = ["-root", "-format", p, "8u", "-set", p, v]

xpropRemove :: MonadIO m => DisplayName -> XPropName -> m ()
xpropRemove d p = do
  specLog $ "xpropRemove running: " ++ show cfg
  runProcess_ cfg
  where
    cfg = xpropProc d ["-root", "-remove", unXPropName p]

xpropList :: MonadIO m => DisplayName -> m ()
xpropList d = liftIO $ do
  specLog $ "xpropList running: " ++ show cfg
  runProcess_ cfg
  where
    cfg = xpropProc d ["-root"]

------------------------------------------------------------------------

withXTerm :: (DisplayName -> IO a) -> (DisplayName -> IO a)
withXTerm action dn = do
  cfg <- makeXterm <$> getSpecLogPriority
  specLog $ "withXTerm running: " ++ show cfg
  withProcessTerm cfg $ const $ do
    threadDelay 500_000 -- give xterm some time to start
    action dn
  where
    makeXterm v = proc "xterm" (displayArg dn) & setStderrCond v

------------------------------------------------------------------------

streamSpecCond :: Priority -> Priority -> StreamSpec any ()
streamSpecCond level verbosity = if level >= verbosity then inherit else nullStream

setStderrCond :: Priority -> ProcessConfig i o e -> ProcessConfig i o ()
setStderrCond = setStderr . streamSpecCond INFO

consumeXDisplayFd :: Handle -> IO DisplayName
consumeXDisplayFd h = do
  l <- readMaybe <$> hGetLine h
  hClose h
  d <- maybe (throwString "Failed to parse display number") (pure . displayNumber) l
  specLog $ "X is on display " ++ show d
  pure d

withXserver :: String -> [String] -> Maybe Int -> (DisplayName -> IO a) -> IO a
withXserver prog args display action = do
  cfg <- makeXvfb <$> getSpecLogPriority
  specLog $ "withXserver running: " ++ show cfg
  withProcessTerm cfg $ \p -> do
    d <- consumeXDisplayFd (getStdout p)
    r <- race (waitExitCode p) (action d)
    either (throwIO . exitException cfg) pure r
  where
    args' = displayArgMaybe ++ ["-displayfd", "1", "-terminate", "60"] ++ args
    makeXvfb logLevel = proc prog args'
      & setCloseFds True
      & setStdin nullStream
      & setStdout createPipe
      & setStderrCond logLevel

    displayArgMaybe = maybeToList (fmap displaySpec display)

    -- If the Xserver exits for whatever reason, before the action
    -- completes, then it's an error.
    exitException p c = ExitCodeException c (setStdout nullStream p) "" ""

withXvfb :: (DisplayName -> IO a) -> IO a
withXvfb = withXserver "Xvfb" ["-screen", "0", "1024x768x24"] Nothing

withXdummy :: (DisplayName -> IO a) -> IO a
withXdummy = withXserver "xdummy" [] Nothing

-- | Using the given 'DisplayName', run an action with the @DISPLAY@
-- environment variable set.
--
-- NB. Don't run tests in parallel if using this. Environment
-- variables are process-global.
setDefaultDisplay_ :: DisplayName -> IO a -> IO a
setDefaultDisplay_ = withSetEnv . displayEnv

-- | Same as 'setDefaultDisplay_', except the 'DisplayName' parameter
-- is passed through to the action.
setDefaultDisplay :: DisplayName -> (DisplayName -> IO a) -> IO a
setDefaultDisplay d action = setDefaultDisplay_ d (action d)

------------------------------------------------------------------------

-- | Adds a guiding phantom type annotation for indices into lists.
-- fixme: zipper rather than indices
newtype ListIndex a = ListIndex { unListIndex :: Int }
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic)

enumerate :: [a] -> [(ListIndex a, a)]
enumerate = zip [0..]

bounds' :: Integral n => (n, n) -> (ListIndex a, ListIndex a)
bounds' = let f = ListIndex . fromIntegral in bimap f f

bounds :: [a] -> (ListIndex a, ListIndex a)
bounds xs = bounds' (0, length xs - 1)

atIndex :: [a] -> ListIndex a -> a
atIndex as (ListIndex i) = as !! i

chooseListIndexN :: Int -> Gen (ListIndex a)
chooseListIndexN n = ListIndex <$> chooseInt (0, n - 1)

chooseListIndex :: [a] -> Gen (ListIndex a)
chooseListIndex = chooseListIndexN . length

------------------------------------------------------------------------

data RRSetup = RRSetup
  { outputs :: [RROutput]
  , primary :: Maybe (ListIndex RROutput)
  , newModes :: [RRMode] -- unused by outputs
  } deriving (Show, Read, Eq, Generic)

instance Default RRSetup where
  def = RRSetup { outputs = [def], primary = Just 0, newModes = [] }

data RROutput = RROutput
  { mode :: Maybe RRExistingMode
  , settings :: RROutputSettings
  , position :: RRPosition
  } deriving (Show, Read, Eq, Generic)

instance Default RROutput where
  def = RROutput { mode = def, settings = def, position = def }

rrOutputOff :: RROutput
rrOutputOff = def { settings = def { disabled = True } }

data RROutputSettings = RROutputSettings
  { disabled :: Bool
  , rotate :: RRRotation
  } deriving (Show, Read, Eq, Generic)

instance Default RROutputSettings where
  def = RROutputSettings { disabled = False, rotate = def }

-- | This is an index into 'modeLines'.
newtype RRExistingMode = RRExistingMode { unRRExistingMode :: ListIndex RRMode }
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Bounded RRExistingMode where
  minBound = RRExistingMode 0
  maxBound = RRExistingMode (snd (bounds modeLines))

instance Default RRExistingMode where
  def = minBound

data RRMode = RRMode
  { name :: RRModeName
  , modeLine :: RRModeLine
  } deriving (Show, Read, Eq, Generic)

instance IsString RRMode where
  fromString = uncurry RRMode . bimap (RRModeName . unquote) RRModeLine . split
    where
      split = second (dropWhile isSpace) . break isSpace
      unquote = takeWhile (/= '"') . dropWhile (== '"')

newtype RRModeName = RRModeName { unRRModeName :: String }
  deriving (Show, Read, Eq, IsString, Generic)

newtype RRModeLine = RRModeLine { unRRModeLine :: String }
  deriving (Show, Read, Eq, IsString, Generic)

data RRPosition = SameAs | RightOf | LeftOf | Below | Above
  deriving (Show, Read, Eq, Bounded, Enum, Generic)

instance Default RRPosition where
  def = SameAs

data RRRotation = Unrotated | RotateLeft | Inverted | RotateRight
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Default RRRotation where
  def = Unrotated

-- | These 'modeLines' are pre-configured in @xdummy@. They can be
-- referred to by name ('RRExistingMode'), or used in 'Arbitrary'
-- instances to generate new modelines.
modeLines :: [RRMode]
modeLines =
  [ "1280x1024 157.500 1280 1344 1504 1728 1024 1025 1028 1072 +HSync +VSync +preferred"
  , "1280x1024 135.000 1280 1296 1440 1688 1024 1025 1028 1066 +HSync +VSync"
  , "1280x1024 108.000 1280 1328 1440 1688 1024 1025 1028 1066 +HSync +VSync"
  , "1280x960 148.500 1280 1344 1504 1728 960 961 964 1011 +HSync +VSync"
  , "1280x960 108.000 1280 1376 1488 1800 960 961 964 1000 +HSync +VSync"
  , "1280x800 83.500 1280 1352 1480 1680 800 803 809 831 -HSync +VSync"
  , "1152x864 108.000 1152 1216 1344 1600 864 865 868 900 +HSync +VSync"
  , "1280x720 74.500 1280 1344 1472 1664 720 723 728 748 -HSync +VSync"
  , "1024x768 94.500 1024 1072 1168 1376 768 769 772 808 +HSync +VSync"
  , "1024x768 78.750 1024 1040 1136 1312 768 769 772 800 +HSync +VSync"
  , "1024x768 75.000 1024 1048 1184 1328 768 771 777 806 -HSync -VSync"
  , "1024x768 65.000 1024 1048 1184 1344 768 771 777 806 -HSync -VSync"
  , "1024x576 46.500 1024 1064 1160 1296 576 579 584 599 -HSync +VSync"
  , "832x624 57.284 832 864 928 1152 624 625 628 667 -HSync -VSync"
  , "960x540 40.750 960 992 1088 1216 540 543 548 562 -HSync +VSync"
  , "800x600 56.300 800 832 896 1048 600 601 604 631 +HSync +VSync"
  , "800x600 50.000 800 856 976 1040 600 637 643 666 +HSync +VSync"
  , "800x600 49.500 800 816 896 1056 600 601 604 625 +HSync +VSync"
  , "800x600 40.000 800 840 968 1056 600 601 605 628 +HSync +VSync"
  , "800x600 36.000 800 824 896 1024 600 601 603 625 +HSync +VSync"
  , "864x486 32.500 864 888 968 1072 486 489 494 506 -HSync +VSync"
  , "640x480 36.000 640 696 752 832 480 481 484 509 -HSync -VSync"
  , "640x480 31.500 640 656 720 840 480 481 484 500 -HSync -VSync"
  , "640x480 31.500 640 664 704 832 480 489 492 520 -HSync -VSync"
  , "640x480 25.175 640 656 752 800 480 490 492 525 -HSync -VSync"
  , "720x400 35.500 720 756 828 936 400 401 404 446 -HSync +VSync"
  , "640x400 31.500 640 672 736 832 400 401 404 445 -HSync +VSync"
  , "640x350 31.500 640 672 736 832 350 382 385 445 +HSync -VSync"
  ]

xrandr :: DisplayName -> [String] -> ProcessConfig () () ()
xrandr d args = proc "xrandr" (displayArg d ++ args)

runXrandr :: DisplayName -> [String] -> IO ()
runXrandr d args = do
  let cfg = xrandr d args
  specLog ("running: " ++ show cfg)
  -- NB. void . readProcess_ instead of runProcess_ so that stderr
  -- can be included in the ExitCodeException.
  void $ readProcess_ cfg

runXrandrSilent :: DisplayName -> [String] -> IO ExitCode
runXrandrSilent d args = do
  let cfg = xrandr d args & setStdout nullStream & setStderr nullStream
  specLog ("running: " ++ show cfg)
  runProcess cfg

withRandrSetup :: DisplayName -> RRSetup -> (IO a -> IO a)
withRandrSetup d rr = bracket_ (randrSetup d rr) (randrTeardown d rr)

randrSetup :: DisplayName -> RRSetup -> IO ()
randrSetup d rr = do
  -- TODO: is it possible to smush these into one xrandr command?
  forM_ rr.newModes $ \m ->
    runXrandr d $ ["--newmode", coerce m.name] ++ words (coerce m.modeLine)

  forM_ (enumerate rr.outputs) $ \(i, o) -> case o.mode of
    Just m -> runXrandr d ["--addmode", outputName i, modeName m]
    Nothing -> pure ()

  runXrandr d args

  where
    args = concat (globalArgs ++ givenArgs ++ switchOffOthers rr.outputs)
    globalArgs = [ ["--noprimary" | isNothing rr.primary ] ]

    givenArgs = zipWith outputArgs [0..] rr.outputs

    outputArgs :: ListIndex RROutput -> RROutput -> [String]
    outputArgs i output =
      [ "--output", outputName i
      , "--rotate", rrRotation output.settings.rotate
      ] ++
      maybe ["--preferred"] (\m -> ["--mode", modeName m]) output.mode ++
      ["--off" | output.settings.disabled ] ++
      ["--primary" | rr.primary == Just i] ++
      (if i > 0 then ["--" ++ rrPosition output.position, outputName (i - 1)] else [])

    switchOffOthers :: [RROutput] -> [[String]]
    switchOffOthers os = [ ["--output", outputName i, "--off"]
                         | i <- [ListIndex (length os)..15] ]

randrTeardown :: DisplayName -> RRSetup -> IO ()
randrTeardown d rr = do
  forM_ (enumerate rr.outputs) $ \(i, o) -> case o.mode of
    Just m -> void $ runXrandrSilent d ["--delmode", outputName i, modeName m]
    Nothing -> pure ()

  forM_ rr.newModes $ \m ->
    void $ runXrandrSilent d ["--rmmode", coerce m.name]

rrRotation :: RRRotation -> String
rrRotation = \case
  Unrotated -> "normal"
  RotateLeft -> "left"
  Inverted -> "inverted"
  RotateRight -> "right"

rrPosition :: RRPosition -> String
rrPosition = \case
  SameAs -> "same-as"
  LeftOf -> "left-of"
  RightOf -> "right-of"
  Above -> "above"
  Below -> "below"

-- | Find the name of the nth RANDR output.
-- Obviously this only works with @xdummy@ config.
outputName :: ListIndex RROutput -> String
outputName (ListIndex i) = "DUMMY" ++ show i

-- | Find the name of a modeline preconfigured in @xdummy@.
modeName :: RRExistingMode -> String
modeName = unRRModeName . name . existingModeName . unRRExistingMode
  where
    existingModeName i = modeLines `atIndex` i

-- | Invent a name for a new X modeline.
newModeName :: ListIndex RRModeLine -> RRModeName
newModeName (ListIndex i) = RRModeName ("newmode" ++ show i)

------------------------------------------------------------------------

-- | Test the test util functions.
spec :: Spec
spec = logSetup $ do
  describe "xvfb" $ aroundAll (withXvfb . withXTerm) $ do
    it "xprop" $ property . prop_xprop
  describe "xdummy" $ aroundAll withXdummy $ do
    it "xprop" $ property . prop_xprop
    it "xrandr" $ property . prop_xrandr

------------------------------------------------------------------------

prop_xprop :: HasCallStack => DisplayName -> XPropName -> XPropValue -> Property
prop_xprop d name value = monadicIO $ do
  xpropSet d name value
  value' <- xpropGet d name
  xpropRemove d name
  pure $ if value /= mempty
    then value' === [value]
    else value' === []

instance Arbitrary XPropName where
  arbitrary = ((:) <$> elements propNameStartChars <*> listOf arbitraryASCIIChar) `suchThatMap` xpropName
  shrink = mapMaybe (xpropName . unXPropName) . genericShrink

instance Arbitrary XPropValue where
  arbitrary = fmap getPrintableString arbitrary `suchThatMap` xpropValue
  shrink = mapMaybe (xpropValue . unXPropValue) . genericShrink

------------------------------------------------------------------------

prop_xrandr :: HasCallStack => DisplayName -> RRSetup -> Property
prop_xrandr d rr = decorate $ monadicIO $ do
    qrr <- run $ withRandrSetup d rr (randrQuery d)
    let (rr', qrr') = reformat rr qrr
    pure $ qrr' === rr'
  where
    decorate =
      tabulate "Outputs" [outputName i | (i, o) <- enumerate rr.outputs, not o.settings.disabled] .
      (if any ((/= Unrotated) . rotate . settings) rr.outputs then label "rotation" else id) .
      (if any (isNothing . mode) rr.outputs then label "using preferred mode" else id) .
      cover 50 (length rr.outputs > 1) "non-trivial"

    reformat rrs = unzip
      . map (bimap snd snd)
      . dropWhileEnd (not . uncurry (||) . bimap fst fst)
      . zip (tuplify (backfill 16 rrs))

    tuplify RRSetup{..} =
      [ (not o.settings.disabled, (i, Just (ListIndex i) == primary))
      | (i, o) <- zip [0..] outputs ]

    backfill n rrs = rrs { outputs = rr.outputs ++ extras }
      where extras = replicate (max 0 (n - length rr.outputs)) rrOutputOff

-- | Scans output of @xrandr --query@ and returns values relevant to
-- test assertions.
randrQuery :: DisplayName -> IO [(Bool, (Int, Bool))]
randrQuery d = parseXrandr <$> readProcessStdout_ (xrandr d ["--query"])
  where
    parseXrandr = mapMaybe parseOutput . BL.lines
    parseOutput line = do
      guard ("DUMMY" `BL.isPrefixOf` line)
      let line' = BL.unpack (BL.drop 5 line)
      (w, ws) <- uncons (words line')
      let e = "connected" `elem` ws
      n <- readMaybe w
      let p = "primary" `elem` ws
      return (e, (n, p))

instance Arbitrary RRSetup where
  arbitrary = do
    nOutputs <- chooseInt (1, 5)

    nNewModes <- chooseInt (1, nOutputs)
    newModeLines <- vectorOf nNewModes (elements (map modeLine modeLines))

    let newModes = [ RRMode (newModeName i) m | (i, m) <- enumerate newModeLines ]

    -- TODO: also use new modes for outputs
    -- mode <- chooseListIndex newModes

    outputs <- vector nOutputs
    primary <- frequency [ (5, Just <$> chooseListIndex outputs)
                         , (1, pure Nothing) ]
    pure $ RRSetup{..}

  shrink rr = do
    (primary, outputs) <- shrinkOutputs
    guard $ not $ null outputs
    pure $ RRSetup {newModes = [], ..}

    where
      shrinkOutputs = case rr.primary of
        Just p -> shrinkListIx shrink (p, rr.outputs)
        Nothing -> map (Nothing,) (shrinkList shrink rr.outputs)

      -- fixme: shrink modes properly too, and adjust mode within outputs
      -- shrinkModes ms = do
      --   ms' <- shrinkListP shrink ms
      --   pure ms'

shrinkListIx :: (a -> [a]) -> (ListIndex a, [a]) -> [(Maybe (ListIndex a), [a])]
shrinkListIx shr (ix, xs) = map unwrap $ shrinkList (shrinkSecond shr) (wrap xs)
  where
    wrap = zip [0..]
    unwrap ixs = let ix' = findIndex ((== ix) . fst) ixs
                 in (fmap ListIndex ix', map snd ixs)

    shrinkSecond :: (a -> [a]) -> (b, a) -> [(b, a)]
    shrinkSecond f (b, a) = map (b,) (f a)

instance Arbitrary RROutput where
  -- Always set a mode because "--preferred" option seems dodgy
  arbitrary = RROutput <$> fmap Just arbitrary <*> arbitrary <*> arbitrary
  shrink o = [ RROutput m s p
             | (m, s, p) <- shrink (o.mode, o.settings, o.position)
             , isJust m
             ]

instance Arbitrary RROutputSettings where
  -- Rotation and reflection don't seem to work for xdummy
  arbitrary = RROutputSettings
    <$> frequency [(5, pure False), (1, pure True)]
    <*> pure Unrotated
  shrink = genericShrink

instance Arbitrary RRExistingMode where
  arbitrary = RRExistingMode <$> chooseListIndex modeLines
  shrink = shrinkMap (RRExistingMode . ListIndex . getPositive) (Positive . unListIndex . unRRExistingMode)

instance Arbitrary RRPosition where
  arbitrary = frequency $ map (second pure)
    [ (2, SameAs)
    , (1, LeftOf)
    , (4, RightOf)
    , (1, Above)
    , (2, Below)
    ]
  shrink = shrinkBoundedEnum

instance Arbitrary RRRotation where
  arbitrary = frequency $ map (second pure)
    [ (4, Unrotated)
    , (2, Inverted)
    , (1, RotateLeft)
    , (1, RotateRight)
    ]
  shrink = shrinkBoundedEnum

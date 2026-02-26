{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import Control.Monad
import qualified DBus as DBus
import DBus.Client
import Data.Char (toLower)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GtkLayerShell as GtkLayerShell
import Graphics.UI.GIGtkStrut
import Options.Applicative
import Paths_gtk_sni_tray (version)
import qualified StatusNotifier.Host.Service as Host
import qualified StatusNotifier.Icon.Pixbuf as IconPixbuf
import StatusNotifier.TransparentWindow
import StatusNotifier.Tray
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.Log.Logger
import System.Posix.Process
import Text.Printf

data Backend = BackendX11 | BackendWayland deriving (Eq, Show)

data BackendChoice = BackendAuto | BackendX11Choice | BackendWaylandChoice
  deriving (Eq, Show, Read)

data IconRecolorMode
  = IconRecolorNone
  | IconRecolorMono
  | IconRecolorDuotone
  deriving (Eq, Show, Read)

detectBackend :: IO Backend
detectBackend = do
  supported <- GtkLayerShell.isSupported
  pure $ if supported then BackendWayland else BackendX11

backendChoiceP :: Parser BackendChoice
backendChoiceP =
  option
    (eitherReader parseBackendChoice)
    ( long "backend"
        <> help "Backend selection: auto | x11 | wayland"
        <> value BackendAuto
        <> metavar "BACKEND"
    )
  where
    parseBackendChoice s =
      case map toLower s of
        "auto" -> Right BackendAuto
        "x11" -> Right BackendX11Choice
        "wayland" -> Right BackendWaylandChoice
        _ -> Left "expected one of: auto, x11, wayland"

logRuntimeInfo :: BackendChoice -> Backend -> IO ()
logRuntimeInfo backendChoice backend = do
  sessionType <- lookupEnv "XDG_SESSION_TYPE"
  waylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
  gdkBackend <- lookupEnv "GDK_BACKEND"
  mDisplay <- Gdk.displayGetDefault
  displayName <- case mDisplay of
    Nothing -> pure Nothing
    Just d -> Just <$> Gdk.displayGetName d
  layerShellSupported <- GtkLayerShell.isSupported
  logM "StatusNotifier.StandaloneWindow" INFO $
    printf
      "backendChoice=%s backend=%s layerShellSupported=%s XDG_SESSION_TYPE=%s WAYLAND_DISPLAY=%s GDK_BACKEND=%s gdkDisplay=%s"
      (show backendChoice)
      (show backend)
      (show layerShellSupported)
      (show sessionType)
      (show waylandDisplay)
      (show gdkBackend)
      (show displayName)

hasStatusNotifierWatcher :: Client -> IO Bool
hasStatusNotifierWatcher client = do
  let mc =
        ( DBus.methodCall
            dbusPath
            (DBus.interfaceName_ "org.freedesktop.DBus")
            (DBus.memberName_ "NameHasOwner")
        )
          { DBus.methodCallDestination = Just dbusName,
            DBus.methodCallBody = [DBus.toVariant ("org.kde.StatusNotifierWatcher" :: String)]
          }
  reply <- call_ client mc
  case DBus.methodReturnBody reply of
    [v] -> pure $ fromMaybe False (DBus.fromVariant v)
    _ -> pure False

setupLayerShellWindow :: StrutConfig -> Gtk.Window -> Bool -> IO ()
setupLayerShellWindow
  StrutConfig
    { strutWidth = widthSize,
      strutHeight = heightSize,
      strutXPadding = xpadding,
      strutYPadding = ypadding,
      strutMonitor = monitorNumber,
      strutPosition = position,
      strutAlignment = alignment,
      strutDisplayName = maybeDisplayName
    }
  window
  reserveSpace = do
    supported <- GtkLayerShell.isSupported
    unless supported $
      logM "StatusNotifier.StandaloneWindow" WARNING $
        "Wayland detected, but gtk-layer-shell is not supported; falling back to a regular toplevel window"
    when supported $ do
      Gtk.windowSetDecorated window False

      maybeDisplay <- maybe Gdk.displayGetDefault Gdk.displayOpen maybeDisplayName
      case maybeDisplay of
        Nothing -> logM "StatusNotifier.StandaloneWindow" WARNING "Failed to get GDK display for layer-shell"
        Just display -> do
          nMonitors <- Gdk.displayGetNMonitors display
          logM "StatusNotifier.StandaloneWindow" INFO $ printf "GDK monitors reported: %d" nMonitors

          let tryIndex idx = if idx < 0 || idx >= nMonitors then pure Nothing else Gdk.displayGetMonitor display idx

          mPrimary <- Gdk.displayGetPrimaryMonitor display
          mChosen <- case monitorNumber of
            Nothing -> pure mPrimary
            Just idx -> tryIndex idx

          monitor <-
            case mChosen <|> mPrimary of
              Just m -> pure (Just m)
              Nothing -> tryIndex 0

          GtkLayerShell.initForWindow window
          GtkLayerShell.setKeyboardMode window GtkLayerShell.KeyboardModeNone
          GtkLayerShell.setNamespace window (T.pack "gtk-sni-tray")
          GtkLayerShell.setLayer window GtkLayerShell.LayerTop

          GtkLayerShell.setMargin window GtkLayerShell.EdgeLeft xpadding
          GtkLayerShell.setMargin window GtkLayerShell.EdgeRight xpadding
          GtkLayerShell.setMargin window GtkLayerShell.EdgeTop ypadding
          GtkLayerShell.setMargin window GtkLayerShell.EdgeBottom ypadding

          let setAnchor = GtkLayerShell.setAnchor window
          case position of
            TopPos -> do
              setAnchor GtkLayerShell.EdgeTop True
              setAnchor GtkLayerShell.EdgeBottom False
              setAnchor GtkLayerShell.EdgeLeft True
              setAnchor GtkLayerShell.EdgeRight True
            BottomPos -> do
              setAnchor GtkLayerShell.EdgeTop False
              setAnchor GtkLayerShell.EdgeBottom True
              setAnchor GtkLayerShell.EdgeLeft True
              setAnchor GtkLayerShell.EdgeRight True
            LeftPos -> do
              setAnchor GtkLayerShell.EdgeLeft True
              setAnchor GtkLayerShell.EdgeRight False
              setAnchor GtkLayerShell.EdgeTop True
              setAnchor GtkLayerShell.EdgeBottom True
            RightPos -> do
              setAnchor GtkLayerShell.EdgeLeft False
              setAnchor GtkLayerShell.EdgeRight True
              setAnchor GtkLayerShell.EdgeTop True
              setAnchor GtkLayerShell.EdgeBottom True

          let fallbackExclusive =
                if reserveSpace
                  then case position of
                    TopPos -> case heightSize of ExactSize h -> h + 2 * ypadding; _ -> 0
                    BottomPos -> case heightSize of ExactSize h -> h + 2 * ypadding; _ -> 0
                    LeftPos -> case widthSize of ExactSize w -> w + 2 * xpadding; _ -> 0
                    RightPos -> case widthSize of ExactSize w -> w + 2 * xpadding; _ -> 0
                  else 0
          GtkLayerShell.setExclusiveZone window fallbackExclusive

          case monitor of
            Nothing -> logM "StatusNotifier.StandaloneWindow" WARNING "Failed to select a GDK monitor for layer-shell; using fallback sizing/anchors"
            Just m -> do
              GtkLayerShell.setMonitor window m
              isPrim <- Gdk.monitorIsPrimary m
              model <- Gdk.monitorGetModel m
              manuf <- Gdk.monitorGetManufacturer m
              logM "StatusNotifier.StandaloneWindow" INFO $
                printf
                  "Using monitor primary=%s manufacturer=%s model=%s"
                  (show isPrim)
                  (show manuf)
                  (show model)

              monitorGeometry <- Gdk.monitorGetGeometry m
              monitorWidth <- Gdk.getRectangleWidth monitorGeometry
              monitorHeight <- Gdk.getRectangleHeight monitorGeometry
              let availableWidth = monitorWidth - (2 * xpadding)
                  availableHeight = monitorHeight - (2 * ypadding)
                  width =
                    case widthSize of
                      ExactSize w -> w
                      ScreenRatio p ->
                        floor $ p * fromIntegral availableWidth
                  height =
                    case heightSize of
                      ExactSize h -> h
                      ScreenRatio p ->
                        floor $ p * fromIntegral availableHeight
                  clampNonNegative x = if x < 0 then 0 else x
                  centerOffset availSize size =
                    clampNonNegative $ (availSize - size) `div` 2
                  endOffset availSize size =
                    clampNonNegative $ availSize - size

                  (leftMargin, rightMargin, topMargin, bottomMargin) =
                    case position of
                      TopPos ->
                        let offset =
                              if width >= availableWidth
                                then 0
                                else case alignment of
                                  Beginning -> 0
                                  Center -> centerOffset availableWidth width
                                  End -> endOffset availableWidth width
                            l = xpadding + offset
                            r = xpadding
                         in (l, r, ypadding, ypadding)
                      BottomPos ->
                        let offset =
                              if width >= availableWidth
                                then 0
                                else case alignment of
                                  Beginning -> 0
                                  Center -> centerOffset availableWidth width
                                  End -> endOffset availableWidth width
                            l = xpadding + offset
                            r = xpadding
                         in (l, r, ypadding, ypadding)
                      LeftPos ->
                        let offset =
                              if height >= availableHeight
                                then 0
                                else case alignment of
                                  Beginning -> 0
                                  Center -> centerOffset availableHeight height
                                  End -> endOffset availableHeight height
                            t = ypadding + offset
                            b = ypadding
                         in (xpadding, xpadding, t, b)
                      RightPos ->
                        let offset =
                              if height >= availableHeight
                                then 0
                                else case alignment of
                                  Beginning -> 0
                                  Center -> centerOffset availableHeight height
                                  End -> endOffset availableHeight height
                            t = ypadding + offset
                            b = ypadding
                         in (xpadding, xpadding, t, b)

                  exclusive =
                    if reserveSpace
                      then case position of
                        TopPos -> height + topMargin
                        BottomPos -> height + bottomMargin
                        LeftPos -> width + leftMargin
                        RightPos -> width + rightMargin
                      else 0

              Gtk.windowSetDefaultSize window (fromIntegral width) (fromIntegral height)
              let (reqWidth, reqHeight) =
                    case position of
                      TopPos -> (min width availableWidth, height)
                      BottomPos -> (min width availableWidth, height)
                      LeftPos -> (width, min height availableHeight)
                      RightPos -> (width, min height availableHeight)
              Gtk.widgetSetSizeRequest
                window
                (fromIntegral reqWidth)
                (fromIntegral reqHeight)

              GtkLayerShell.setMargin window GtkLayerShell.EdgeLeft leftMargin
              GtkLayerShell.setMargin window GtkLayerShell.EdgeRight rightMargin
              GtkLayerShell.setMargin window GtkLayerShell.EdgeTop topMargin
              GtkLayerShell.setMargin window GtkLayerShell.EdgeBottom bottomMargin

              case position of
                TopPos -> do
                  setAnchor GtkLayerShell.EdgeTop True
                  setAnchor GtkLayerShell.EdgeBottom False
                  if width >= availableWidth
                    then do
                      setAnchor GtkLayerShell.EdgeLeft True
                      setAnchor GtkLayerShell.EdgeRight True
                    else case alignment of
                      Beginning -> do
                        setAnchor GtkLayerShell.EdgeLeft True
                        setAnchor GtkLayerShell.EdgeRight False
                      Center -> do
                        setAnchor GtkLayerShell.EdgeLeft True
                        setAnchor GtkLayerShell.EdgeRight False
                      End -> do
                        setAnchor GtkLayerShell.EdgeLeft False
                        setAnchor GtkLayerShell.EdgeRight True
                BottomPos -> do
                  setAnchor GtkLayerShell.EdgeTop False
                  setAnchor GtkLayerShell.EdgeBottom True
                  if width >= availableWidth
                    then do
                      setAnchor GtkLayerShell.EdgeLeft True
                      setAnchor GtkLayerShell.EdgeRight True
                    else case alignment of
                      Beginning -> do
                        setAnchor GtkLayerShell.EdgeLeft True
                        setAnchor GtkLayerShell.EdgeRight False
                      Center -> do
                        setAnchor GtkLayerShell.EdgeLeft True
                        setAnchor GtkLayerShell.EdgeRight False
                      End -> do
                        setAnchor GtkLayerShell.EdgeLeft False
                        setAnchor GtkLayerShell.EdgeRight True
                LeftPos -> do
                  setAnchor GtkLayerShell.EdgeLeft True
                  setAnchor GtkLayerShell.EdgeRight False
                  if height >= availableHeight
                    then do
                      setAnchor GtkLayerShell.EdgeTop True
                      setAnchor GtkLayerShell.EdgeBottom True
                    else case alignment of
                      Beginning -> do
                        setAnchor GtkLayerShell.EdgeTop True
                        setAnchor GtkLayerShell.EdgeBottom False
                      Center -> do
                        setAnchor GtkLayerShell.EdgeTop True
                        setAnchor GtkLayerShell.EdgeBottom False
                      End -> do
                        setAnchor GtkLayerShell.EdgeTop False
                        setAnchor GtkLayerShell.EdgeBottom True
                RightPos -> do
                  setAnchor GtkLayerShell.EdgeLeft False
                  setAnchor GtkLayerShell.EdgeRight True
                  if height >= availableHeight
                    then do
                      setAnchor GtkLayerShell.EdgeTop True
                      setAnchor GtkLayerShell.EdgeBottom True
                    else case alignment of
                      Beginning -> do
                        setAnchor GtkLayerShell.EdgeTop True
                        setAnchor GtkLayerShell.EdgeBottom False
                      Center -> do
                        setAnchor GtkLayerShell.EdgeTop True
                        setAnchor GtkLayerShell.EdgeBottom False
                      End -> do
                        setAnchor GtkLayerShell.EdgeTop False
                        setAnchor GtkLayerShell.EdgeBottom True

              GtkLayerShell.setExclusiveZone window exclusive

positionP :: Parser StrutPosition
positionP =
  fromMaybe TopPos
    <$> optional
      ( flag'
          TopPos
          ( long "top"
              <> help "Position the bar at the top of the screen"
          )
          <|> flag'
            BottomPos
            ( long "bottom"
                <> help "Position the bar at the bottom of the screen"
            )
          <|> flag'
            LeftPos
            ( long "left"
                <> help "Position the bar on the left side of the screen"
            )
          <|> flag'
            RightPos
            ( long "right"
                <> help "Position the bar on the right side of the screen"
            )
      )

alignmentP :: Parser StrutAlignment
alignmentP =
  fromMaybe Center
    <$> optional
      ( flag'
          Beginning
          ( long "beginning"
              <> help "Use beginning alignment"
          )
          <|> flag'
            Center
            ( long "center"
                <> help "Use center alignment"
            )
          <|> flag'
            End
            ( long "end"
                <> help "Use end alignment"
            )
      )

sizeP :: Parser Int32
sizeP =
  option
    auto
    ( long "size"
        <> short 's'
        <> help "Set the size of the bar"
        <> value 30
        <> metavar "SIZE"
    )

paddingP :: Parser Int32
paddingP =
  option
    auto
    ( long "padding"
        <> short 'p'
        <> help "Set the padding of the bar"
        <> value 0
        <> metavar "PADDING"
    )

monitorNumberP :: Parser [Int32]
monitorNumberP =
  many $
    option
      auto
      ( long "monitor"
          <> short 'm'
          <> help "Display a tray bar on the given monitor"
          <> metavar "MONITOR"
      )

logP :: Parser Priority
logP =
  option
    auto
    ( long "log-level"
        <> short 'l'
        <> help "Set the log level"
        <> metavar "LEVEL"
        <> value WARNING
    )

colorP :: Parser (Maybe String)
colorP =
  optional $
    strOption
      ( long "color"
          <> short 'c'
          <> help "Set the background color of the tray; See https://developer.gnome.org/gdk3/stable/gdk3-RGBA-Colors.html#gdk-rgba-parse for acceptable values"
          <> metavar "COLOR"
      )

expandP :: Parser Bool
expandP =
  switch
    ( long "expand"
        <> help "Let icons expand into the space allocated to the tray"
        <> short 'e'
    )

centerIconsP :: Parser Bool
centerIconsP =
  switch
    ( long "center-icons"
        <> help "Center the tray icons within the bar"
    )

startWatcherP :: Parser Bool
startWatcherP =
  switch
    ( long "watcher"
        <> short 'w'
        <> help "Start a Watcher to handle SNI registration if one does not exist"
    )

noStrutP :: Parser Bool
noStrutP =
  switch
    ( long "no-strut"
        <> help "Do not reserve space for the window (X11: no strut; Wayland: exclusive zone 0)"
    )

barLengthP :: Parser Rational
barLengthP =
  option
    auto
    ( long "length"
        <> help "Set the proportion of the screen that the tray bar should occupy -- values are parsed as haskell rationals (e.g. 1 % 2)"
        <> value 1
    )

overlayScaleP :: Parser Rational
overlayScaleP =
  option
    auto
    ( long "overlay-scale"
        <> short 'o'
        <> help "The proportion of the tray icon's size that should be set for overlay icons."
        <> value (5 % 7)
    )

iconPreferenceP :: Parser TrayIconPreference
iconPreferenceP =
  option
    (eitherReader parsePref)
    ( long "icon-preference"
        <> help "Icon preference when both are provided: pixmaps (default) | themed"
        <> value PreferPixmaps
        <> metavar "PREFERENCE"
    )
  where
    parsePref s =
      case map toLower s of
        "pixmaps" -> Right PreferPixmaps
        "pixmap" -> Right PreferPixmaps
        "themed" -> Right PreferThemedIcons
        "theme" -> Right PreferThemedIcons
        _ -> Left "expected one of: pixmaps, themed"

iconRecolorModeP :: Parser IconRecolorMode
iconRecolorModeP =
  option
    (eitherReader parseMode)
    ( long "icon-recolor"
        <> help "Recolor icons based on their alpha mask: none (default) | mono | duotone"
        <> value IconRecolorNone
        <> metavar "MODE"
    )
  where
    parseMode s =
      case map toLower s of
        "none" -> Right IconRecolorNone
        "mono" -> Right IconRecolorMono
        "duo" -> Right IconRecolorDuotone
        "duotone" -> Right IconRecolorDuotone
        _ -> Left "expected one of: none, mono, duotone"

menuBackendP :: Parser MenuBackend
menuBackendP =
  option
    (eitherReader parseMenuBackend)
    ( long "menu-backend"
        <> help "Menu backend: haskell (default) | libdbusmenu"
        <> value HaskellDBusMenu
        <> metavar "BACKEND"
    )
  where
    parseMenuBackend s =
      case map toLower s of
        "libdbusmenu" -> Right LibDBusMenu
        "haskell" -> Right HaskellDBusMenu
        _ -> Left "expected one of: libdbusmenu, haskell"

getColor :: String -> IO Gdk.RGBA
getColor colorString = do
  rgba <- Gdk.newZeroRGBA
  colorParsed <- Gdk.rGBAParse rgba (T.pack colorString)
  unless colorParsed $ do
    logM "StatusNotifier.Tray" WARNING "Failed to parse provided color"
    void $ Gdk.rGBAParse rgba "#000000"
  return rgba

buildWindows ::
  StrutPosition ->
  StrutAlignment ->
  Int32 ->
  Int32 ->
  [Int32] ->
  Priority ->
  BackendChoice ->
  Maybe String ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Rational ->
  Rational ->
  MenuBackend ->
  TrayIconPreference ->
  IconRecolorMode ->
  IO ()
buildWindows
  pos
  align
  size
  padding
  monitors
  priority
  backendChoice
  maybeColorString
  expand
  centerIcons
  startWatcher
  noStrut
  barLength
  overlayScale
  menuBackend
  iconPreference
  iconRecolorMode = do
    _ <- Gtk.init Nothing
    logger <- getLogger "StatusNotifier"
    saveGlobalLogger $ setLevel priority logger
    detectedBackend <- detectBackend
    let backend =
          case backendChoice of
            BackendAuto -> detectedBackend
            BackendX11Choice -> BackendX11
            BackendWaylandChoice -> BackendWayland
    client <- connectSession
    logRuntimeInfo backendChoice backend
    watcherPresent <- hasStatusNotifierWatcher client
    unless watcherPresent $ do
      logM "StatusNotifier" WARNING $
        "No StatusNotifierWatcher found on D-Bus (org.kde.StatusNotifierWatcher). Tray will likely be empty."
      unless startWatcher $
        logM "StatusNotifier" WARNING $
          "Start a watcher first (recommended) or run with --watcher to start one in-process."
    _ <- getRootLogger
    pid <- getProcessID
    host <-
      Host.build
        Host.defaultParams
          { Host.dbusClient = Just client,
            Host.uniqueIdentifier = printf "standalone-%s" $ show pid,
            Host.startWatcher = startWatcher
          }
        >>= maybe (logM "StatusNotifier" ERROR "Failed to start StatusNotifier host" >> exitFailure) pure
    initialItems <- Host.itemInfoMap host
    logM "StatusNotifier" INFO $ printf "Initial tray items: %d" (Map.size initialItems)
    let c1 =
          defaultStrutConfig
            { strutPosition = pos,
              strutAlignment = align,
              strutXPadding = padding,
              strutYPadding = padding
            }
        defaultRatio = ScreenRatio barLength
        configBase =
          case pos of
            TopPos -> c1 {strutHeight = ExactSize size, strutWidth = defaultRatio}
            BottomPos ->
              c1 {strutHeight = ExactSize size, strutWidth = defaultRatio}
            RightPos ->
              c1 {strutHeight = defaultRatio, strutWidth = ExactSize size}
            LeftPos ->
              c1 {strutHeight = defaultRatio, strutWidth = ExactSize size}
        buildWithConfig config = do
          let orientation =
                case strutPosition config of
                  TopPos -> Gtk.OrientationHorizontal
                  BottomPos -> Gtk.OrientationHorizontal
                  _ -> Gtk.OrientationVertical
              mixWord8 t a b =
                let ta = fromIntegral a :: Double
                    tb = fromIntegral b :: Double
                    out = ta + max 0 (min 1 t) * (tb - ta)
                 in fromIntegral (max (0 :: Int) (min 255 (round out)))
              mixRgb8 t (IconPixbuf.Rgb8 r g b) (IconPixbuf.Rgb8 r2 g2 b2) =
                IconPixbuf.Rgb8 (mixWord8 t r r2) (mixWord8 t g g2) (mixWord8 t b b2)
              mkIconTransform =
                case iconRecolorMode of
                  IconRecolorNone -> Nothing
                  IconRecolorMono ->
                    Just $ \image pb -> do
                      ctx <- Gtk.widgetGetStyleContext image
                      fg <- Gtk.styleContextGetColor ctx [Gtk.StateFlagsNormal]
                      fg8 <- IconPixbuf.rgb8FromGdkRGBA fg
                      mpb <- IconPixbuf.recolorPixbufMonochrome fg8 pb
                      return $ fromMaybe pb mpb
                  IconRecolorDuotone ->
                    Just $ \image pb -> do
                      ctx <- Gtk.widgetGetStyleContext image
                      fg <- Gtk.styleContextGetColor ctx [Gtk.StateFlagsNormal]
                      fg8 <- IconPixbuf.rgb8FromGdkRGBA fg
                      let black = IconPixbuf.Rgb8 0 0 0
                          white = IconPixbuf.Rgb8 255 255 255
                          dark = mixRgb8 0.35 fg8 black
                          light = mixRgb8 0.35 fg8 white
                      mpb <- IconPixbuf.recolorPixbufDuotone dark light pb
                      return $ fromMaybe pb mpb
          tray <-
            buildTray
              host
              client
              TrayParams
                { trayOrientation = orientation,
                  trayImageSize = Expand,
                  trayIconExpand = expand,
                  trayIconPreference = iconPreference,
                  trayAlignment = align,
                  trayOverlayScale = overlayScale,
                  trayLeftClickAction = Activate,
                  trayMiddleClickAction = SecondaryActivate,
                  trayRightClickAction = PopupMenu,
                  trayMenuBackend = menuBackend,
                  trayCenterIcons = centerIcons,
                  trayPriorityConfig = defaultTrayPriorityConfig,
                  trayPixbufTransform = mkIconTransform,
                  trayEventHooks = defaultTrayEventHooks
                }
          window <- Gtk.windowNew Gtk.WindowTypeToplevel
          Gtk.windowSetResizable window False
          Gtk.windowSetSkipTaskbarHint window True
          Gtk.windowSetSkipPagerHint window True
          Gtk.windowSetAcceptFocus window False
          Gtk.windowSetFocusOnMap window False
          Gtk.windowSetKeepAbove window True
          Gtk.windowSetTypeHint window Gdk.WindowTypeHintDock
          case backend of
            BackendX11 ->
              when (not noStrut) $ setupStrutWindow config window
            BackendWayland ->
              setupLayerShellWindow config window (not noStrut)
          maybe
            (makeWindowTransparent window)
            ( getColor
                >=> Gtk.widgetOverrideBackgroundColor window [Gtk.StateFlagsNormal]
                  . Just
            )
            maybeColorString
          Gtk.containerAdd window tray
          Gtk.widgetShowAll window
        runForMonitor monitor =
          buildWithConfig configBase {strutMonitor = Just monitor}
    if null monitors
      then buildWithConfig configBase
      else mapM_ runForMonitor monitors
    Gtk.main

parser :: Parser (IO ())
parser =
  buildWindows
    <$> positionP
    <*> alignmentP
    <*> sizeP
    <*> paddingP
    <*> monitorNumberP
    <*> logP
    <*> backendChoiceP
    <*> colorP
    <*> expandP
    <*> centerIconsP
    <*> startWatcherP
    <*> noStrutP
    <*> barLengthP
    <*> overlayScaleP
    <*> menuBackendP
    <*> iconPreferenceP
    <*> iconRecolorModeP

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (printf "gtk-sni-tray-standalone %s" $ showVersion version)
    ( long "version"
        <> help "Show the version number of gtk-sni-tray"
    )

main :: IO ()
main =
  join $
    execParser $
      info
        (helper <*> versionOption <*> parser)
        ( fullDesc
            <> progDesc "Run a standalone StatusNotifierItem/AppIndicator tray"
        )

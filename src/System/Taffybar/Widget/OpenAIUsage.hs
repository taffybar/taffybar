{-# LANGUAGE OverloadedStrings #-}

-- | Widgets for displaying OpenAI Codex subscription usage.
module System.Taffybar.Widget.OpenAIUsage
  ( OpenAIUsageDisplayMode (..),
    OpenAIUsageDisplayModeState,
    OpenAIUsageLabelConfig (..),
    OpenAIUsageStackConfig (..),
    OpenAIUsageWindowSelector (..),
    defaultOpenAIUsageLabelConfig,
    defaultOpenAIUsageStackConfig,
    openAIUsageLabelNew,
    openAIUsageLabelNewWith,
    openAIUsagePrimaryWindowLabelNew,
    openAIUsageSecondaryWindowLabelNew,
    openAIUsageSectionNewWith,
    openAIUsageStackNew,
    openAIUsageStackNewWith,
    openAIUsageNew,
    openAIUsageNewWith,
    formatOpenAIUsageWindowLabel,
    formatOpenAIUsageSummaryLabel,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, swapTVar)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.OpenAIUsage
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import Text.Printf (printf)

data OpenAIUsageDisplayMode
  = OpenAIUsageDisplayUsed
  | OpenAIUsageDisplayRemaining
  deriving (Eq, Show)

newtype OpenAIUsageDisplayModeState
  = OpenAIUsageDisplayModeState
      (MVar OpenAIUsageDisplayMode, TChan OpenAIUsageDisplayMode)

data OpenAIUsageLabelParts = OpenAIUsageLabelParts
  { openAIUsageLabelWidget :: Gtk.Widget,
    openAIUsageLabel :: Gtk.Label,
    openAIUsageLabelDisplayState :: OpenAIUsageDisplayModeState,
    openAIUsageLabelSnapshotVar :: MVar OpenAIUsageSnapshot,
    openAIUsageLabelUsageChan :: TChan OpenAIUsageSnapshot,
    openAIUsageLabelRefreshNow :: IO ()
  }

data OpenAIUsageLabelConfig = OpenAIUsageLabelConfig
  { openAIUsageLabelInfoConfig :: OpenAIUsageConfig,
    openAIUsageLabelDefaultDisplayMode :: OpenAIUsageDisplayMode,
    openAIUsageLabelFallbackText :: T.Text,
    openAIUsageLabelClass :: T.Text,
    openAIUsageLabelFormatter :: OpenAIUsageDisplayMode -> OpenAIUsageInfo -> T.Text,
    openAIUsageLabelUnavailableFormatter :: T.Text -> T.Text,
    openAIUsageLabelTooltipFormatter :: OpenAIUsageDisplayMode -> OpenAIUsageSnapshot -> Maybe T.Text
  }

data OpenAIUsageWindowSelector
  = OpenAIUsagePrimaryWindow
  | OpenAIUsageSecondaryWindow
  deriving (Eq, Show)

data OpenAIUsageStackConfig = OpenAIUsageStackConfig
  { openAIUsageStackInfoConfig :: OpenAIUsageConfig,
    openAIUsageStackDefaultDisplayMode :: OpenAIUsageDisplayMode,
    openAIUsageStackFallbackText :: T.Text
  }

defaultOpenAIUsageLabelConfig :: OpenAIUsageLabelConfig
defaultOpenAIUsageLabelConfig =
  OpenAIUsageLabelConfig
    { openAIUsageLabelInfoConfig = defaultOpenAIUsageConfig,
      openAIUsageLabelDefaultDisplayMode = OpenAIUsageDisplayUsed,
      openAIUsageLabelFallbackText = "AI n/a",
      openAIUsageLabelClass = "openai-usage-label",
      openAIUsageLabelFormatter = formatOpenAIUsageSummaryLabel,
      openAIUsageLabelUnavailableFormatter = const "AI n/a",
      openAIUsageLabelTooltipFormatter = formatOpenAIUsageTooltip
    }

defaultOpenAIUsageStackConfig :: OpenAIUsageStackConfig
defaultOpenAIUsageStackConfig =
  OpenAIUsageStackConfig
    { openAIUsageStackInfoConfig = defaultOpenAIUsageConfig,
      openAIUsageStackDefaultDisplayMode = OpenAIUsageDisplayUsed,
      openAIUsageStackFallbackText = "n/a"
    }

openAIUsageNew :: TaffyIO Gtk.Widget
openAIUsageNew = openAIUsageLabelNew

openAIUsageNewWith :: OpenAIUsageLabelConfig -> TaffyIO Gtk.Widget
openAIUsageNewWith = openAIUsageLabelNewWith

openAIUsageLabelNew :: TaffyIO Gtk.Widget
openAIUsageLabelNew = openAIUsageLabelNewWith defaultOpenAIUsageLabelConfig

openAIUsagePrimaryWindowLabelNew :: TaffyIO Gtk.Widget
openAIUsagePrimaryWindowLabelNew =
  openAIUsageLabelNewWith $
    windowLabelConfig OpenAIUsagePrimaryWindow defaultOpenAIUsageStackConfig

openAIUsageSecondaryWindowLabelNew :: TaffyIO Gtk.Widget
openAIUsageSecondaryWindowLabelNew =
  openAIUsageLabelNewWith $
    windowLabelConfig OpenAIUsageSecondaryWindow defaultOpenAIUsageStackConfig

openAIUsageStackNew :: TaffyIO Gtk.Widget
openAIUsageStackNew = openAIUsageStackNewWith defaultOpenAIUsageStackConfig

openAIUsageStackNewWith :: OpenAIUsageStackConfig -> TaffyIO Gtk.Widget
openAIUsageStackNewWith config = do
  (stack, primaryParts) <- openAIUsageStackPartsNewWith config
  liftIO $
    wrapOpenAIUsageMenu
      "openai-usage"
      stack
      (windowLabelConfig OpenAIUsagePrimaryWindow config)
      primaryParts

openAIUsageSectionNewWith :: Gtk.Widget -> OpenAIUsageStackConfig -> TaffyIO Gtk.Widget
openAIUsageSectionNewWith iconWidget config = do
  (stack, primaryParts) <- openAIUsageStackPartsNewWith config
  liftIO $ do
    section <- buildIconLabelBox iconWidget stack
    _ <- widgetSetClassGI section "usage-section"
    wrapOpenAIUsageMenu
      "openai-usage"
      section
      (windowLabelConfig OpenAIUsagePrimaryWindow config)
      primaryParts

openAIUsageStackPartsNewWith :: OpenAIUsageStackConfig -> TaffyIO (Gtk.Widget, OpenAIUsageLabelParts)
openAIUsageStackPartsNewWith config = do
  primaryParts <- openAIUsageLabelPartsNewWith $ windowLabelConfig OpenAIUsagePrimaryWindow config
  secondaryParts <- openAIUsageLabelPartsNewWith $ windowLabelConfig OpenAIUsageSecondaryWindow config
  liftIO $ do
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    _ <- widgetSetClassGI box "openai-usage-stack"
    Gtk.boxPackStart box (openAIUsageLabelWidget primaryParts) False False 0
    Gtk.boxPackStart box (openAIUsageLabelWidget secondaryParts) False False 0
    Gtk.widgetShowAll box
    widget <- Gtk.toWidget box
    return (widget, primaryParts)

windowLabelConfig :: OpenAIUsageWindowSelector -> OpenAIUsageStackConfig -> OpenAIUsageLabelConfig
windowLabelConfig selector stackConfig =
  defaultOpenAIUsageLabelConfig
    { openAIUsageLabelInfoConfig = openAIUsageStackInfoConfig stackConfig,
      openAIUsageLabelDefaultDisplayMode = openAIUsageStackDefaultDisplayMode stackConfig,
      openAIUsageLabelFallbackText = openAIUsageStackFallbackText stackConfig,
      openAIUsageLabelClass = "openai-usage-window-label",
      openAIUsageLabelFormatter = formatOpenAIUsageWindowLabel selector,
      openAIUsageLabelUnavailableFormatter = const (openAIUsageStackFallbackText stackConfig)
    }

openAIUsageLabelNewWith :: OpenAIUsageLabelConfig -> TaffyIO Gtk.Widget
openAIUsageLabelNewWith config = do
  parts <- openAIUsageLabelPartsNewWith config
  liftIO $ wrapOpenAIUsageMenu "openai-usage" (openAIUsageLabelWidget parts) config parts

openAIUsageLabelPartsNewWith :: OpenAIUsageLabelConfig -> TaffyIO OpenAIUsageLabelParts
openAIUsageLabelPartsNewWith config = do
  let infoConfig = openAIUsageLabelInfoConfig config
  usageChan <- getOpenAIUsageChan infoConfig
  initialSnapshot <- getOpenAIUsageState infoConfig
  displayState <- getOpenAIUsageDisplayModeState (openAIUsageLabelDefaultDisplayMode config)
  ctx <- ask

  liftIO $ do
    label <- Gtk.labelNew (Just (openAIUsageLabelFallbackText config))
    snapshotVar <- newMVar initialSnapshot
    let refreshNow = runReaderT (forceOpenAIUsageRefresh infoConfig) ctx
    _ <- widgetSetClassGI label (openAIUsageLabelClass config)
    updateLabelFromState config label displayState initialSnapshot

    void $ Gtk.onWidgetRealize label $ do
      usageThread <- forkUsageListener config label displayState snapshotVar usageChan
      modeThread <- forkDisplayModeListener config label displayState snapshotVar
      void $ Gtk.onWidgetUnrealize label $ do
        killThread usageThread
        killThread modeThread

    Gtk.widgetShowAll label
    widget <- Gtk.toWidget label
    return $
      OpenAIUsageLabelParts
        { openAIUsageLabelWidget = widget,
          openAIUsageLabel = label,
          openAIUsageLabelDisplayState = displayState,
          openAIUsageLabelSnapshotVar = snapshotVar,
          openAIUsageLabelUsageChan = usageChan,
          openAIUsageLabelRefreshNow = refreshNow
        }

wrapOpenAIUsageMenu ::
  T.Text ->
  Gtk.Widget ->
  OpenAIUsageLabelConfig ->
  OpenAIUsageLabelParts ->
  IO Gtk.Widget
wrapOpenAIUsageMenu klass child config parts = do
  ebox <- Gtk.eventBoxNew
  _ <- widgetSetClassGI ebox klass
  Gtk.containerAdd ebox child
  initialSnapshot <- readMVar (openAIUsageLabelSnapshotVar parts)
  menu <-
    buildUsageMenu
      ebox
      config
      (openAIUsageLabel parts)
      (openAIUsageLabelDisplayState parts)
      (openAIUsageLabelSnapshotVar parts)
      (openAIUsageLabelRefreshNow parts)
      initialSnapshot
  menuVar <- newTVarIO menu

  let rebuildMenu snapshot = do
        newMenu <-
          buildUsageMenu
            ebox
            config
            (openAIUsageLabel parts)
            (openAIUsageLabelDisplayState parts)
            (openAIUsageLabelSnapshotVar parts)
            (openAIUsageLabelRefreshNow parts)
            snapshot
        oldMenu <- atomically $ swapTVar menuVar newMenu
        -- Attached menus hold a GObject reference, so the replaced menu
        -- must be destroyed explicitly or it (and its item tree) leaks on
        -- every usage snapshot.
        Gtk.widgetDestroy oldMenu

  void $ Gtk.onWidgetRealize ebox $ do
    menuThread <-
      forkCachedMenuUpdater
        rebuildMenu
        (openAIUsageLabelSnapshotVar parts)
        (openAIUsageLabelUsageChan parts)
    void $ Gtk.onWidgetUnrealize ebox $ killThread menuThread

  void $
    Gtk.onWidgetButtonPressEvent ebox $ \_event -> do
      currentEvent <- Gtk.getCurrentEvent
      readyMenu <- readTVarIO menuVar
      Gtk.widgetShowAll readyMenu
      Gtk.menuPopupAtPointer readyMenu currentEvent
      void $ forkIO (openAIUsageLabelRefreshNow parts)
      return True
  Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

buildUsageMenu ::
  Gtk.EventBox ->
  OpenAIUsageLabelConfig ->
  Gtk.Label ->
  OpenAIUsageDisplayModeState ->
  MVar OpenAIUsageSnapshot ->
  IO () ->
  OpenAIUsageSnapshot ->
  IO Gtk.Menu
buildUsageMenu anchor config label displayState snapshotVar refreshNow snapshot = do
  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu anchor Nothing
  appendUsageMenuContents
    menu
    config
    label
    displayState
    snapshotVar
    refreshNow
    snapshot
  return menu

getOpenAIUsageDisplayModeState :: OpenAIUsageDisplayMode -> TaffyIO OpenAIUsageDisplayModeState
getOpenAIUsageDisplayModeState defaultMode =
  getStateDefault $ liftIO $ do
    modeVar <- newMVar defaultMode
    modeChan <- newBroadcastTChanIO
    return $ OpenAIUsageDisplayModeState (modeVar, modeChan)

forkUsageListener ::
  OpenAIUsageLabelConfig ->
  Gtk.Label ->
  OpenAIUsageDisplayModeState ->
  MVar OpenAIUsageSnapshot ->
  TChan OpenAIUsageSnapshot ->
  IO ThreadId
forkUsageListener config label displayState snapshotVar usageChan = do
  ourUsageChan <- atomically $ dupTChan usageChan
  forkIO $
    forever $ do
      snapshot <- atomically $ readTChan ourUsageChan
      void $ swapMVar snapshotVar snapshot
      updateLabelFromState config label displayState snapshot

forkDisplayModeListener ::
  OpenAIUsageLabelConfig ->
  Gtk.Label ->
  OpenAIUsageDisplayModeState ->
  MVar OpenAIUsageSnapshot ->
  IO ThreadId
forkDisplayModeListener config label displayState@(OpenAIUsageDisplayModeState (_, modeChan)) snapshotVar = do
  ourModeChan <- atomically $ dupTChan modeChan
  forkIO $
    forever $ do
      _ <- atomically $ readTChan ourModeChan
      snapshot <- readMVar snapshotVar
      updateLabelFromState config label displayState snapshot

updateLabelFromState ::
  OpenAIUsageLabelConfig ->
  Gtk.Label ->
  OpenAIUsageDisplayModeState ->
  OpenAIUsageSnapshot ->
  IO ()
updateLabelFromState config label displayState snapshot = do
  displayMode <- readDisplayMode displayState
  let labelText = formatSnapshotLabel config displayMode snapshot
      tooltipText = openAIUsageLabelTooltipFormatter config displayMode snapshot
  postGUIASync $ do
    Gtk.labelSetText label labelText
    Gtk.widgetSetTooltipText label tooltipText

formatSnapshotLabel :: OpenAIUsageLabelConfig -> OpenAIUsageDisplayMode -> OpenAIUsageSnapshot -> T.Text
formatSnapshotLabel config mode snapshot =
  case snapshot of
    OpenAIUsageAvailable info -> openAIUsageLabelFormatter config mode info
    OpenAIUsageUnavailable message -> openAIUsageLabelUnavailableFormatter config message

readDisplayMode :: OpenAIUsageDisplayModeState -> IO OpenAIUsageDisplayMode
readDisplayMode (OpenAIUsageDisplayModeState (modeVar, _)) = readMVar modeVar

setDisplayMode :: OpenAIUsageDisplayModeState -> OpenAIUsageDisplayMode -> IO ()
setDisplayMode (OpenAIUsageDisplayModeState (modeVar, modeChan)) mode = do
  void $ swapMVar modeVar mode
  atomically $ writeTChan modeChan mode

toggleDisplayMode :: OpenAIUsageDisplayMode -> OpenAIUsageDisplayMode
toggleDisplayMode OpenAIUsageDisplayUsed = OpenAIUsageDisplayRemaining
toggleDisplayMode OpenAIUsageDisplayRemaining = OpenAIUsageDisplayUsed

formatOpenAIUsageSummaryLabel :: OpenAIUsageDisplayMode -> OpenAIUsageInfo -> T.Text
formatOpenAIUsageSummaryLabel displayMode info =
  let limit = openAIUsageRateLimit info
      windows =
        T.intercalate
          " "
          [ formatSelectedWindowLabel OpenAIUsagePrimaryWindow displayMode limit,
            formatSelectedWindowLabel OpenAIUsageSecondaryWindow displayMode limit
          ]
      reached = openAIUsageLimitReached limit || isJust (openAIUsageReachedType info)
   in (if reached then "AI ! " else "AI ") <> windows

formatOpenAIUsageWindowLabel :: OpenAIUsageWindowSelector -> OpenAIUsageDisplayMode -> OpenAIUsageInfo -> T.Text
formatOpenAIUsageWindowLabel selector displayMode info =
  formatSelectedWindowLabel selector displayMode (openAIUsageRateLimit info)

formatSelectedWindowLabel :: OpenAIUsageWindowSelector -> OpenAIUsageDisplayMode -> OpenAIUsageRateLimit -> T.Text
formatSelectedWindowLabel selector displayMode limit =
  case selectedWindow selector limit of
    Nothing
      | selector == OpenAIUsagePrimaryWindow && shortLimitDisabled limit ->
          windowSelectorFallbackName selector <> " ∞"
      | otherwise -> windowSelectorFallbackName selector <> " n/a"
    Just window -> formatWindowLabel displayMode (windowSelectorFallbackName selector) window

formatOpenAIUsageTooltip :: OpenAIUsageDisplayMode -> OpenAIUsageSnapshot -> Maybe T.Text
formatOpenAIUsageTooltip displayMode snapshot =
  Just $
    case snapshot of
      OpenAIUsageUnavailable message -> "Unable to fetch OpenAI usage: " <> message
      OpenAIUsageAvailable info ->
        T.intercalate "\n" $
          catMaybes
            [ ("Plan: " <>) <$> openAIUsagePlanType info,
              Just $ "Display: " <> displayModeText displayMode,
              Just $ "Default: " <> formatRateLimit displayMode (openAIUsageRateLimit info),
              formatAdditionalTooltip displayMode (openAIUsageAdditionalRateLimits info),
              formatCredits <$> openAIUsageCredits info,
              ("Reached: " <>) <$> openAIUsageReachedType info
            ]

forkCachedMenuUpdater ::
  (OpenAIUsageSnapshot -> IO ()) ->
  MVar OpenAIUsageSnapshot ->
  TChan OpenAIUsageSnapshot ->
  IO ThreadId
forkCachedMenuUpdater rebuildMenu snapshotVar usageChan = do
  ourUsageChan <- atomically $ dupTChan usageChan
  forkIO $
    forever $ do
      snapshot <- atomically $ readTChan ourUsageChan
      void $ swapMVar snapshotVar snapshot
      void $
        GLib.idleAdd GLib.PRIORITY_LOW $ do
          rebuildMenu snapshot
          return False

appendUsageMenuContents ::
  Gtk.Menu ->
  OpenAIUsageLabelConfig ->
  Gtk.Label ->
  OpenAIUsageDisplayModeState ->
  MVar OpenAIUsageSnapshot ->
  IO () ->
  OpenAIUsageSnapshot ->
  IO ()
appendUsageMenuContents menu config label displayState snapshotVar refreshNow snapshot = do
  displayMode <- readDisplayMode displayState
  appendInfoItem menu "OpenAI Codex usage"
  appendSnapshotMenuItems menu displayMode snapshot

  appendSeparator menu
  toggleItem <-
    Gtk.menuItemNewWithLabel $
      "Display: "
        <> displayModeText displayMode
        <> " (toggle)"
  void $
    Gtk.onMenuItemActivate toggleItem $ do
      setDisplayMode displayState (toggleDisplayMode displayMode)
      snapshot' <- readMVar snapshotVar
      updateLabelFromState config label displayState snapshot'
  Gtk.menuShellAppend menu toggleItem

  refreshItem <- Gtk.menuItemNewWithLabel ("Refresh" :: T.Text)
  void $ Gtk.onMenuItemActivate refreshItem $ void $ forkIO refreshNow
  Gtk.menuShellAppend menu refreshItem

appendInfoItem :: Gtk.Menu -> T.Text -> IO ()
appendInfoItem menu text = do
  item <- Gtk.menuItemNewWithLabel text
  Gtk.widgetSetSensitive item False
  Gtk.menuShellAppend menu item

appendSeparator :: Gtk.Menu -> IO ()
appendSeparator menu = do
  sep <- Gtk.separatorMenuItemNew
  Gtk.menuShellAppend menu sep

appendSnapshotMenuItems :: Gtk.Menu -> OpenAIUsageDisplayMode -> OpenAIUsageSnapshot -> IO ()
appendSnapshotMenuItems menu _ (OpenAIUsageUnavailable message) =
  appendMenuSection menu "Status" ["Unavailable: " <> message]
appendSnapshotMenuItems menu displayMode (OpenAIUsageAvailable info) = do
  appendMenuSection
    menu
    "Account"
    ( catMaybes
        [ ("Plan: " <>) <$> openAIUsagePlanType info,
          Just $ "Display: " <> displayModeText displayMode
        ]
    )
  appendMenuSection
    menu
    "Default Limit"
    (formatRateLimitMenuLines displayMode (openAIUsageRateLimit info))
  appendMenuSection
    menu
    "Credits"
    (catMaybes [formatCredits <$> openAIUsageCredits info])
  appendMenuSection
    menu
    "Limit Reached"
    (catMaybes [("Type: " <>) <$> openAIUsageReachedType info])
  forM_ (openAIUsageAdditionalRateLimits info) $ \additional ->
    appendMenuSection
      menu
      (fromMaybeText "Additional Limit" (openAIUsageAdditionalLimitName additional))
      (formatRateLimitMenuLines displayMode (openAIUsageAdditionalRateLimit additional))

appendMenuSection :: Gtk.Menu -> T.Text -> [T.Text] -> IO ()
appendMenuSection _ _ [] = return ()
appendMenuSection menu title items = do
  appendSeparator menu
  appendInfoItem menu title
  forM_ items (appendInfoItem menu)

formatRateLimitMenuLines :: OpenAIUsageDisplayMode -> OpenAIUsageRateLimit -> [T.Text]
formatRateLimitMenuLines displayMode limit =
  ["5h: unlimited" | shortLimitDisabled limit]
    <> catMaybes
      [ formatWindowMenuLine displayMode "5h" <$> selectedWindow OpenAIUsagePrimaryWindow limit,
        formatWindowMenuLine displayMode "7d" <$> selectedWindow OpenAIUsageSecondaryWindow limit
      ]
    <> concat
      ( catMaybes
          [ formatWindowTokenMenuLines "5h" <$> selectedWindow OpenAIUsagePrimaryWindow limit,
            formatWindowTokenMenuLines "7d" <$> selectedWindow OpenAIUsageSecondaryWindow limit
          ]
      )
    <> [formatRateLimitStatus limit]

formatWindowMenuLine :: OpenAIUsageDisplayMode -> T.Text -> OpenAIUsageWindow -> T.Text
formatWindowMenuLine displayMode fallbackName window =
  formatWindowName fallbackName window
    <> ": "
    <> formatWindowValue displayMode window
    <> " "
    <> displayModeText displayMode
    <> formatWindowPercentSuffix displayMode window
    <> maybe "" ((", resets in " <>) . formatDuration) (openAIUsageResetAfterSeconds window)

formatWindowTokenMenuLines :: T.Text -> OpenAIUsageWindow -> [T.Text]
formatWindowTokenMenuLines fallbackName window =
  case openAIUsageWindowTotals window of
    Nothing -> []
    Just totals ->
      [ formatWindowName fallbackName window <> " tokens: " <> formatTotalTokens totals,
        formatWindowName fallbackName window <> " requests: " <> T.pack (show $ openAIUsageRequestCount totals),
        formatWindowName fallbackName window <> " input: " <> formatCount (openAIUsageInputTokens totals),
        formatWindowName fallbackName window <> " cached input: " <> formatCount (openAIUsageCachedInputTokens totals),
        formatWindowName fallbackName window <> " output: " <> formatCount (openAIUsageOutputTokens totals),
        formatWindowName fallbackName window <> " reasoning output: " <> formatCount (openAIUsageReasoningOutputTokens totals)
      ]

formatRateLimitStatus :: OpenAIUsageRateLimit -> T.Text
formatRateLimitStatus limit
  | openAIUsageLimitReached limit = "Status: reached"
  | not (openAIUsageAllowed limit) = "Status: blocked"
  | otherwise = "Status: allowed"

formatAdditionalTooltip :: OpenAIUsageDisplayMode -> [OpenAIUsageAdditionalRateLimit] -> Maybe T.Text
formatAdditionalTooltip _ [] = Nothing
formatAdditionalTooltip displayMode additional =
  Just $ T.intercalate "\n" $ map (formatAdditional displayMode) additional

formatAdditional :: OpenAIUsageDisplayMode -> OpenAIUsageAdditionalRateLimit -> T.Text
formatAdditional displayMode additional =
  fromMaybeText "Additional" (openAIUsageAdditionalLimitName additional)
    <> ": "
    <> formatRateLimit displayMode (openAIUsageAdditionalRateLimit additional)

formatRateLimit :: OpenAIUsageDisplayMode -> OpenAIUsageRateLimit -> T.Text
formatRateLimit displayMode limit =
  let windows =
        T.intercalate
          ", "
          ( ["5h unlimited" | shortLimitDisabled limit]
              <> mapMaybe
                (uncurry (formatWindow displayMode))
                [ ("short", selectedWindow OpenAIUsagePrimaryWindow limit),
                  ("long", selectedWindow OpenAIUsageSecondaryWindow limit)
                ]
          )
      status
        | openAIUsageLimitReached limit = "reached"
        | not (openAIUsageAllowed limit) = "blocked"
        | otherwise = "allowed"
   in windows <> " (" <> status <> ")"

formatWindow :: OpenAIUsageDisplayMode -> T.Text -> Maybe OpenAIUsageWindow -> Maybe T.Text
formatWindow _ _ Nothing = Nothing
formatWindow displayMode name (Just window) =
  Just $
    formatWindowName name window
      <> " "
      <> formatWindowValue displayMode window
      <> " "
      <> displayModeText displayMode
      <> formatWindowPercentSuffix displayMode window
      <> maybe "" ((" / " <>) . formatDuration) (openAIUsageWindowDurationSeconds window)
      <> maybe "" ((", resets in " <>) . formatDuration) (openAIUsageResetAfterSeconds window)

formatWindowLabel :: OpenAIUsageDisplayMode -> T.Text -> OpenAIUsageWindow -> T.Text
formatWindowLabel displayMode fallbackName window =
  formatWindowName fallbackName window
    <> " "
    <> formatWindowValueWithIndicator displayMode window

formatWindowName :: T.Text -> OpenAIUsageWindow -> T.Text
formatWindowName fallbackName window =
  maybe fallbackName formatDuration (openAIUsageWindowDurationSeconds window)

formatWindowPercent :: OpenAIUsageDisplayMode -> OpenAIUsageWindow -> T.Text
formatWindowPercent displayMode window =
  T.pack $ printf "%d%%" $ displayPercent displayMode window

formatWindowValue :: OpenAIUsageDisplayMode -> OpenAIUsageWindow -> T.Text
formatWindowValue = formatWindowPercent

formatWindowValueWithIndicator :: OpenAIUsageDisplayMode -> OpenAIUsageWindow -> T.Text
formatWindowValueWithIndicator displayMode window =
  formatWindowValue displayMode window <> displayModeIndicator displayMode

formatWindowPercentSuffix :: OpenAIUsageDisplayMode -> OpenAIUsageWindow -> T.Text
formatWindowPercentSuffix OpenAIUsageDisplayUsed window
  | isJust (openAIUsageWindowTotals window) = " (tokens in menu)"
formatWindowPercentSuffix _ _ = ""

displayPercent :: OpenAIUsageDisplayMode -> OpenAIUsageWindow -> Int
displayPercent OpenAIUsageDisplayUsed = openAIUsageUsedPercent
displayPercent OpenAIUsageDisplayRemaining = max 0 . (100 -) . openAIUsageUsedPercent

displayModeText :: OpenAIUsageDisplayMode -> T.Text
displayModeText OpenAIUsageDisplayUsed = "used"
displayModeText OpenAIUsageDisplayRemaining = "remaining"

displayModeIndicator :: OpenAIUsageDisplayMode -> T.Text
displayModeIndicator OpenAIUsageDisplayUsed = "u"
displayModeIndicator OpenAIUsageDisplayRemaining = "r"

formatCredits :: OpenAIUsageCredits -> T.Text
formatCredits credits =
  "Credits: "
    <> ( if openAIUsageCreditsUnlimited credits
           then "unlimited"
           else fromMaybeText "0" (openAIUsageCreditsBalance credits)
       )
    <> if openAIUsageHasCredits credits then " available" else ""

formatTotalTokens :: OpenAIUsageTotals -> T.Text
formatTotalTokens = formatCount . openAIUsageTotalTokens

formatCount :: Int -> T.Text
formatCount count
  | count >= 1_000_000 = T.pack $ printf "%.1fM" (fromIntegral count / 1_000_000 :: Double)
  | count >= 10_000 = T.pack $ printf "%dk" (count `div` 1_000)
  | count >= 1_000 = T.pack $ printf "%.1fk" (fromIntegral count / 1_000 :: Double)
  | otherwise = T.pack $ show count

selectedWindow :: OpenAIUsageWindowSelector -> OpenAIUsageRateLimit -> Maybe OpenAIUsageWindow
selectedWindow OpenAIUsagePrimaryWindow limit
  | primaryWindowIsLong limit && isNothing (openAIUsageSecondaryWindow limit) = Nothing
  | otherwise = openAIUsagePrimaryWindow limit
selectedWindow OpenAIUsageSecondaryWindow limit =
  case openAIUsageSecondaryWindow limit of
    Just window -> Just window
    Nothing
      | primaryWindowIsLong limit -> openAIUsagePrimaryWindow limit
      | otherwise -> Nothing

-- Codex temporarily omits the 5-hour window when that limit is disabled. In
-- that state the 7-day window moves into the API's primary slot instead of
-- leaving a placeholder there, so identify the window by its duration before
-- assigning it to the widget's semantic 5-hour and 7-day rows.
primaryWindowIsLong :: OpenAIUsageRateLimit -> Bool
primaryWindowIsLong limit =
  case openAIUsagePrimaryWindow limit >>= openAIUsageWindowDurationSeconds of
    Just duration -> duration >= 24 * 60 * 60
    Nothing -> False

shortLimitDisabled :: OpenAIUsageRateLimit -> Bool
shortLimitDisabled limit =
  primaryWindowIsLong limit && isNothing (openAIUsageSecondaryWindow limit)

windowSelectorFallbackName :: OpenAIUsageWindowSelector -> T.Text
windowSelectorFallbackName OpenAIUsagePrimaryWindow = "5h"
windowSelectorFallbackName OpenAIUsageSecondaryWindow = "7d"

formatDuration :: Int -> T.Text
formatDuration seconds
  | seconds < 60 = T.pack $ printf "%ds" seconds
  | seconds < 3600 = T.pack $ printf "%dm" (seconds `div` 60)
  | seconds < 86400 = T.pack $ printf "%dh" (seconds `div` 3600)
  | otherwise = T.pack $ printf "%dd" (seconds `div` 86400)

fromMaybeText :: T.Text -> Maybe T.Text -> T.Text
fromMaybeText = fromMaybe

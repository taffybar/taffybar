{-# LANGUAGE OverloadedStrings #-}

-- | Widgets for displaying Claude Code subscription usage.
module System.Taffybar.Widget.AnthropicUsage
  ( AnthropicUsageDisplayMode (..),
    AnthropicUsageDisplayModeState,
    AnthropicUsageLabelConfig (..),
    AnthropicUsageStackConfig (..),
    AnthropicUsageWindowSelector (..),
    defaultAnthropicUsageLabelConfig,
    defaultAnthropicUsageStackConfig,
    anthropicUsageLabelNew,
    anthropicUsageLabelNewWith,
    anthropicUsageFiveHourWindowLabelNew,
    anthropicUsageWeeklyWindowLabelNew,
    anthropicUsageSectionNewWith,
    anthropicUsageStackNew,
    anthropicUsageStackNewWith,
    anthropicUsageNew,
    anthropicUsageNewWith,
    formatAnthropicUsageWindowLabel,
    formatAnthropicUsageSummaryLabel,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Clock
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.AnthropicUsage
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import Text.Printf (printf)

data AnthropicUsageDisplayMode
  = AnthropicUsageDisplayUsed
  | AnthropicUsageDisplayRemaining
  deriving (Eq, Show)

newtype AnthropicUsageDisplayModeState
  = AnthropicUsageDisplayModeState
      (MVar AnthropicUsageDisplayMode, TChan AnthropicUsageDisplayMode)

data AnthropicUsageLabelParts = AnthropicUsageLabelParts
  { anthropicUsageLabelWidget :: Gtk.Widget,
    anthropicUsageLabel :: Gtk.Label,
    anthropicUsageLabelDisplayState :: AnthropicUsageDisplayModeState,
    anthropicUsageLabelSnapshotVar :: MVar AnthropicUsageSnapshot,
    anthropicUsageLabelUsageChan :: TChan AnthropicUsageSnapshot,
    anthropicUsageLabelRefreshNow :: IO ()
  }

data AnthropicUsageLabelConfig = AnthropicUsageLabelConfig
  { anthropicUsageLabelInfoConfig :: AnthropicUsageConfig,
    anthropicUsageLabelDefaultDisplayMode :: AnthropicUsageDisplayMode,
    anthropicUsageLabelFallbackText :: T.Text,
    anthropicUsageLabelClass :: T.Text,
    anthropicUsageLabelFormatter :: AnthropicUsageDisplayMode -> AnthropicUsageInfo -> T.Text,
    anthropicUsageLabelUnavailableFormatter :: T.Text -> T.Text,
    anthropicUsageLabelTooltipFormatter :: AnthropicUsageDisplayMode -> AnthropicUsageSnapshot -> Maybe T.Text
  }

data AnthropicUsageWindowSelector
  = AnthropicUsageFiveHourWindow
  | AnthropicUsageWeeklyWindow
  | -- | Per-model weekly limit (currently Fable). Absent for plans without one,
    -- in which case the label renders empty.
    AnthropicUsageScopedWeeklyWindow
  deriving (Eq, Show)

data AnthropicUsageStackConfig = AnthropicUsageStackConfig
  { anthropicUsageStackInfoConfig :: AnthropicUsageConfig,
    anthropicUsageStackDefaultDisplayMode :: AnthropicUsageDisplayMode,
    anthropicUsageStackFallbackText :: T.Text
  }

defaultAnthropicUsageLabelConfig :: AnthropicUsageLabelConfig
defaultAnthropicUsageLabelConfig =
  AnthropicUsageLabelConfig
    { anthropicUsageLabelInfoConfig = defaultAnthropicUsageConfig,
      anthropicUsageLabelDefaultDisplayMode = AnthropicUsageDisplayUsed,
      anthropicUsageLabelFallbackText = "Claude n/a",
      anthropicUsageLabelClass = "anthropic-usage-label",
      anthropicUsageLabelFormatter = formatAnthropicUsageSummaryLabel,
      anthropicUsageLabelUnavailableFormatter = const "Claude n/a",
      anthropicUsageLabelTooltipFormatter = formatAnthropicUsageTooltip
    }

defaultAnthropicUsageStackConfig :: AnthropicUsageStackConfig
defaultAnthropicUsageStackConfig =
  AnthropicUsageStackConfig
    { anthropicUsageStackInfoConfig = defaultAnthropicUsageConfig,
      anthropicUsageStackDefaultDisplayMode = AnthropicUsageDisplayUsed,
      anthropicUsageStackFallbackText = "n/a"
    }

anthropicUsageNew :: TaffyIO Gtk.Widget
anthropicUsageNew = anthropicUsageLabelNew

anthropicUsageNewWith :: AnthropicUsageLabelConfig -> TaffyIO Gtk.Widget
anthropicUsageNewWith = anthropicUsageLabelNewWith

anthropicUsageLabelNew :: TaffyIO Gtk.Widget
anthropicUsageLabelNew = anthropicUsageLabelNewWith defaultAnthropicUsageLabelConfig

anthropicUsageFiveHourWindowLabelNew :: TaffyIO Gtk.Widget
anthropicUsageFiveHourWindowLabelNew =
  anthropicUsageLabelNewWith $
    windowLabelConfig AnthropicUsageFiveHourWindow defaultAnthropicUsageStackConfig

anthropicUsageWeeklyWindowLabelNew :: TaffyIO Gtk.Widget
anthropicUsageWeeklyWindowLabelNew =
  anthropicUsageLabelNewWith $
    windowLabelConfig AnthropicUsageWeeklyWindow defaultAnthropicUsageStackConfig

anthropicUsageStackNew :: TaffyIO Gtk.Widget
anthropicUsageStackNew = anthropicUsageStackNewWith defaultAnthropicUsageStackConfig

anthropicUsageStackNewWith :: AnthropicUsageStackConfig -> TaffyIO Gtk.Widget
anthropicUsageStackNewWith config = do
  (stack, fiveHourParts) <- anthropicUsageStackPartsNewWith config
  liftIO $
    wrapAnthropicUsageMenu
      "anthropic-usage"
      stack
      (windowLabelConfig AnthropicUsageFiveHourWindow config)
      fiveHourParts

anthropicUsageSectionNewWith :: Gtk.Widget -> AnthropicUsageStackConfig -> TaffyIO Gtk.Widget
anthropicUsageSectionNewWith iconWidget config = do
  (stack, fiveHourParts) <- anthropicUsageStackPartsNewWith config
  liftIO $ do
    section <- buildIconLabelBox iconWidget stack
    _ <- widgetSetClassGI section "usage-section"
    wrapAnthropicUsageMenu
      "anthropic-usage"
      section
      (windowLabelConfig AnthropicUsageFiveHourWindow config)
      fiveHourParts

anthropicUsageStackPartsNewWith :: AnthropicUsageStackConfig -> TaffyIO (Gtk.Widget, AnthropicUsageLabelParts)
anthropicUsageStackPartsNewWith config = do
  fiveHourParts <- anthropicUsageLabelPartsNewWith $ windowLabelConfig AnthropicUsageFiveHourWindow config
  weeklyParts <- anthropicUsageLabelPartsNewWith $ windowLabelConfig AnthropicUsageWeeklyWindow config
  scopedParts <- anthropicUsageLabelPartsNewWith $ windowLabelConfig AnthropicUsageScopedWeeklyWindow config
  liftIO $ do
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    _ <- widgetSetClassGI box "anthropic-usage-stack"
    Gtk.boxPackStart box (anthropicUsageLabelWidget fiveHourParts) False False 0
    Gtk.boxPackStart box (anthropicUsageLabelWidget weeklyParts) False False 0
    Gtk.boxPackStart box (anthropicUsageLabelWidget scopedParts) False False 0
    Gtk.widgetShowAll box
    widget <- Gtk.toWidget box
    return (widget, fiveHourParts)

windowLabelConfig :: AnthropicUsageWindowSelector -> AnthropicUsageStackConfig -> AnthropicUsageLabelConfig
windowLabelConfig selector stackConfig =
  defaultAnthropicUsageLabelConfig
    { anthropicUsageLabelInfoConfig = anthropicUsageStackInfoConfig stackConfig,
      anthropicUsageLabelDefaultDisplayMode = anthropicUsageStackDefaultDisplayMode stackConfig,
      anthropicUsageLabelFallbackText = anthropicUsageStackFallbackText stackConfig,
      anthropicUsageLabelClass = "anthropic-usage-window-label",
      anthropicUsageLabelFormatter = formatAnthropicUsageWindowLabel selector,
      anthropicUsageLabelUnavailableFormatter = const (anthropicUsageStackFallbackText stackConfig)
    }

anthropicUsageLabelNewWith :: AnthropicUsageLabelConfig -> TaffyIO Gtk.Widget
anthropicUsageLabelNewWith config = do
  parts <- anthropicUsageLabelPartsNewWith config
  liftIO $ wrapAnthropicUsageMenu "anthropic-usage" (anthropicUsageLabelWidget parts) config parts

anthropicUsageLabelPartsNewWith :: AnthropicUsageLabelConfig -> TaffyIO AnthropicUsageLabelParts
anthropicUsageLabelPartsNewWith config = do
  let infoConfig = anthropicUsageLabelInfoConfig config
  usageChan <- getAnthropicUsageChan infoConfig
  initialSnapshot <- getAnthropicUsageState infoConfig
  displayState <- getAnthropicUsageDisplayModeState (anthropicUsageLabelDefaultDisplayMode config)
  ctx <- ask

  liftIO $ do
    label <- Gtk.labelNew (Just (anthropicUsageLabelFallbackText config))
    snapshotVar <- newMVar initialSnapshot
    let refreshNow = runReaderT (forceAnthropicUsageRefresh infoConfig) ctx
    -- Visibility is driven by 'updateLabelFromState': a formatter returning
    -- empty text (e.g. the scoped per-model window when the plan has none)
    -- hides the label entirely instead of leaving an empty row, and
    -- no-show-all keeps ancestor 'widgetShowAll' calls from re-showing it.
    Gtk.widgetSetNoShowAll label True
    _ <- widgetSetClassGI label (anthropicUsageLabelClass config)
    updateLabelFromState config label displayState initialSnapshot

    void $ Gtk.onWidgetRealize label $ do
      -- The usage listener only runs while the label is realized (e.g. the
      -- visible page of a GtkStack), so broadcasts that arrived while the
      -- widget was hidden were missed. Re-sync from the shared state before
      -- listening so the label does not show a stale snapshot.
      currentSnapshot <- runReaderT (getAnthropicUsageState infoConfig) ctx
      void $ swapMVar snapshotVar currentSnapshot
      updateLabelFromState config label displayState currentSnapshot
      usageThread <- forkUsageListener config label displayState snapshotVar usageChan
      modeThread <- forkDisplayModeListener config label displayState snapshotVar
      void $ Gtk.onWidgetUnrealize label $ do
        killThread usageThread
        killThread modeThread

    Gtk.widgetShowAll label
    widget <- Gtk.toWidget label
    return $
      AnthropicUsageLabelParts
        { anthropicUsageLabelWidget = widget,
          anthropicUsageLabel = label,
          anthropicUsageLabelDisplayState = displayState,
          anthropicUsageLabelSnapshotVar = snapshotVar,
          anthropicUsageLabelUsageChan = usageChan,
          anthropicUsageLabelRefreshNow = refreshNow
        }

wrapAnthropicUsageMenu ::
  T.Text ->
  Gtk.Widget ->
  AnthropicUsageLabelConfig ->
  AnthropicUsageLabelParts ->
  IO Gtk.Widget
wrapAnthropicUsageMenu klass child config parts = do
  ebox <- Gtk.eventBoxNew
  _ <- widgetSetClassGI ebox klass
  Gtk.containerAdd ebox child
  void $
    Gtk.onWidgetButtonPressEvent ebox $ \_event -> do
      showUsageMenu
        ebox
        config
        (anthropicUsageLabel parts)
        (anthropicUsageLabelDisplayState parts)
        (anthropicUsageLabelSnapshotVar parts)
        (anthropicUsageLabelUsageChan parts)
        (anthropicUsageLabelRefreshNow parts)
      return True
  Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

getAnthropicUsageDisplayModeState :: AnthropicUsageDisplayMode -> TaffyIO AnthropicUsageDisplayModeState
getAnthropicUsageDisplayModeState defaultMode =
  getStateDefault $ liftIO $ do
    modeVar <- newMVar defaultMode
    modeChan <- newBroadcastTChanIO
    return $ AnthropicUsageDisplayModeState (modeVar, modeChan)

forkUsageListener ::
  AnthropicUsageLabelConfig ->
  Gtk.Label ->
  AnthropicUsageDisplayModeState ->
  MVar AnthropicUsageSnapshot ->
  TChan AnthropicUsageSnapshot ->
  IO ThreadId
forkUsageListener config label displayState snapshotVar usageChan = do
  ourUsageChan <- atomically $ dupTChan usageChan
  forkIO $
    forever $ do
      snapshot <- atomically $ readTChan ourUsageChan
      void $ swapMVar snapshotVar snapshot
      updateLabelFromState config label displayState snapshot

forkDisplayModeListener ::
  AnthropicUsageLabelConfig ->
  Gtk.Label ->
  AnthropicUsageDisplayModeState ->
  MVar AnthropicUsageSnapshot ->
  IO ThreadId
forkDisplayModeListener config label displayState@(AnthropicUsageDisplayModeState (_, modeChan)) snapshotVar = do
  ourModeChan <- atomically $ dupTChan modeChan
  forkIO $
    forever $ do
      _ <- atomically $ readTChan ourModeChan
      snapshot <- readMVar snapshotVar
      updateLabelFromState config label displayState snapshot

updateLabelFromState ::
  AnthropicUsageLabelConfig ->
  Gtk.Label ->
  AnthropicUsageDisplayModeState ->
  AnthropicUsageSnapshot ->
  IO ()
updateLabelFromState config label displayState snapshot = do
  displayMode <- readDisplayMode displayState
  let labelText = formatSnapshotLabel config displayMode snapshot
      tooltipText = anthropicUsageLabelTooltipFormatter config displayMode snapshot
  postGUIASync $ do
    Gtk.labelSetText label labelText
    Gtk.widgetSetTooltipText label tooltipText
    Gtk.widgetSetVisible label (not (T.null labelText))

formatSnapshotLabel :: AnthropicUsageLabelConfig -> AnthropicUsageDisplayMode -> AnthropicUsageSnapshot -> T.Text
formatSnapshotLabel config mode snapshot =
  case snapshot of
    AnthropicUsageAvailable info -> anthropicUsageLabelFormatter config mode info
    AnthropicUsageUnavailable message -> anthropicUsageLabelUnavailableFormatter config message

readDisplayMode :: AnthropicUsageDisplayModeState -> IO AnthropicUsageDisplayMode
readDisplayMode (AnthropicUsageDisplayModeState (modeVar, _)) = readMVar modeVar

setDisplayMode :: AnthropicUsageDisplayModeState -> AnthropicUsageDisplayMode -> IO ()
setDisplayMode (AnthropicUsageDisplayModeState (modeVar, modeChan)) mode = do
  void $ swapMVar modeVar mode
  atomically $ writeTChan modeChan mode

toggleDisplayMode :: AnthropicUsageDisplayMode -> AnthropicUsageDisplayMode
toggleDisplayMode AnthropicUsageDisplayUsed = AnthropicUsageDisplayRemaining
toggleDisplayMode AnthropicUsageDisplayRemaining = AnthropicUsageDisplayUsed

formatAnthropicUsageSummaryLabel :: AnthropicUsageDisplayMode -> AnthropicUsageInfo -> T.Text
formatAnthropicUsageSummaryLabel displayMode info =
  let windows =
        T.intercalate " " $
          [ formatWindowLabel displayMode (anthropicUsageFiveHourWindow info),
            formatWindowLabel displayMode (anthropicUsageWeeklyWindow info)
          ]
            <> maybe
              []
              (\window -> [formatWindowLabel displayMode window])
              (anthropicUsageScopedWeeklyWindow info)
      unavailable = anthropicUsageHasAvailableSubscription info == Just False
   in (if unavailable then "Claude ! " else "Claude ") <> windows

formatAnthropicUsageWindowLabel :: AnthropicUsageWindowSelector -> AnthropicUsageDisplayMode -> AnthropicUsageInfo -> T.Text
formatAnthropicUsageWindowLabel selector displayMode info =
  maybe "" (formatWindowLabel displayMode) (selectedWindow selector info)

formatAnthropicUsageTooltip :: AnthropicUsageDisplayMode -> AnthropicUsageSnapshot -> Maybe T.Text
formatAnthropicUsageTooltip displayMode snapshot =
  Just $
    case snapshot of
      AnthropicUsageUnavailable message -> "Unable to read Claude usage: " <> message
      AnthropicUsageAvailable info ->
        T.intercalate "\n" $
          catMaybes
            [ ("Plan: " <>) <$> anthropicUsageSubscriptionType info,
              ("Tier: " <>) <$> anthropicUsageRateLimitTier info,
              ("Organization: " <>) <$> anthropicUsageOrganizationName info,
              Just $ "Display: " <> displayModeText displayMode,
              Just $ "5h: " <> formatWindow displayMode (anthropicUsageGeneratedAt info) (anthropicUsageFiveHourWindow info),
              Just $ "7d: " <> formatWindow displayMode (anthropicUsageGeneratedAt info) (anthropicUsageWeeklyWindow info),
              ( \window ->
                  anthropicUsageWindowName window
                    <> ": "
                    <> formatWindow displayMode (anthropicUsageGeneratedAt info) window
              )
                <$> anthropicUsageScopedWeeklyWindow info,
              formatSubscriptionStatus info,
              ("Extra usage disabled: " <>) <$> anthropicUsageExtraUsageDisabledReason info
            ]

showUsageMenu ::
  Gtk.EventBox ->
  AnthropicUsageLabelConfig ->
  Gtk.Label ->
  AnthropicUsageDisplayModeState ->
  MVar AnthropicUsageSnapshot ->
  TChan AnthropicUsageSnapshot ->
  IO () ->
  IO ()
showUsageMenu anchor config label displayState snapshotVar usageChan refreshNow = do
  currentEvent <- Gtk.getCurrentEvent
  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu anchor Nothing
  populateUsageMenu menu config label displayState snapshotVar refreshNow

  ourUsageChan <- atomically $ dupTChan usageChan
  menuThread <-
    forkIO $
      forever $ do
        snapshot <- atomically $ readTChan ourUsageChan
        void $ swapMVar snapshotVar snapshot
        postGUIASync $ populateUsageMenu menu config label displayState snapshotVar refreshNow

  void $
    Gtk.onWidgetHide menu $
      void $
        GLib.idleAdd GLib.PRIORITY_LOW $ do
          killThread menuThread
          Gtk.widgetDestroy menu
          return False

  Gtk.widgetShowAll menu
  Gtk.menuPopupAtPointer menu currentEvent
  void $ forkIO refreshNow

populateUsageMenu ::
  Gtk.Menu ->
  AnthropicUsageLabelConfig ->
  Gtk.Label ->
  AnthropicUsageDisplayModeState ->
  MVar AnthropicUsageSnapshot ->
  IO () ->
  IO ()
populateUsageMenu menu config label displayState snapshotVar refreshNow = do
  children <- Gtk.containerGetChildren menu
  forM_ children $ \item -> do
    Gtk.containerRemove menu item
    Gtk.widgetDestroy item
  snapshot <- readMVar snapshotVar
  displayMode <- readDisplayMode displayState
  appendInfoItem menu "Claude Code usage"
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
      readMVar snapshotVar >>= updateLabelFromState config label displayState
      populateUsageMenu menu config label displayState snapshotVar refreshNow
  Gtk.menuShellAppend menu toggleItem

  refreshItem <- Gtk.menuItemNewWithLabel ("Refresh" :: T.Text)
  void $ Gtk.onMenuItemActivate refreshItem $ void $ forkIO refreshNow
  Gtk.menuShellAppend menu refreshItem

  Gtk.widgetShowAll menu

appendInfoItem :: Gtk.Menu -> T.Text -> IO ()
appendInfoItem menu text = do
  item <- Gtk.menuItemNewWithLabel text
  Gtk.widgetSetSensitive item False
  Gtk.menuShellAppend menu item

appendSeparator :: Gtk.Menu -> IO ()
appendSeparator menu = do
  sep <- Gtk.separatorMenuItemNew
  Gtk.menuShellAppend menu sep

appendSnapshotMenuItems :: Gtk.Menu -> AnthropicUsageDisplayMode -> AnthropicUsageSnapshot -> IO ()
appendSnapshotMenuItems menu _ (AnthropicUsageUnavailable message) =
  appendMenuSection menu "Status" ["Unavailable: " <> message]
appendSnapshotMenuItems menu displayMode (AnthropicUsageAvailable info) = do
  appendMenuSection
    menu
    "Account"
    ( catMaybes
        [ ("Plan: " <>) <$> anthropicUsageSubscriptionType info,
          ("Tier: " <>) <$> anthropicUsageRateLimitTier info,
          ("Organization: " <>) <$> anthropicUsageOrganizationName info,
          formatSubscriptionStatus info,
          Just $ "Display: " <> displayModeText displayMode
        ]
    )
  appendMenuSection
    menu
    "5h Window"
    (formatWindowMenuLines displayMode (anthropicUsageGeneratedAt info) (anthropicUsageFiveHourWindow info))
  appendMenuSection
    menu
    "7d Window"
    (formatWindowMenuLines displayMode (anthropicUsageGeneratedAt info) (anthropicUsageWeeklyWindow info))
  forM_ (anthropicUsageScopedWeeklyWindow info) $ \window ->
    appendMenuSection
      menu
      (anthropicUsageWindowName window <> " Window")
      (formatWindowMenuLines displayMode (anthropicUsageGeneratedAt info) window)
  appendMenuSection
    menu
    "Extra Usage"
    (catMaybes [("Disabled: " <>) <$> anthropicUsageExtraUsageDisabledReason info])

appendMenuSection :: Gtk.Menu -> T.Text -> [T.Text] -> IO ()
appendMenuSection _ _ [] = return ()
appendMenuSection menu title items = do
  appendSeparator menu
  appendInfoItem menu title
  forM_ items (appendInfoItem menu)

formatWindowMenuLines :: AnthropicUsageDisplayMode -> UTCTime -> AnthropicUsageWindow -> [T.Text]
formatWindowMenuLines displayMode generatedAt window =
  [ "Tokens: " <> formatTotalTokens (anthropicUsageWindowTotals window),
    "Requests: " <> T.pack (show $ anthropicUsageRequestCount $ anthropicUsageWindowTotals window),
    "Input: " <> formatCount (anthropicUsageInputTokens totals),
    "Cache write: " <> formatCount (anthropicUsageCacheCreationTokens totals),
    "Cache read: " <> formatCount (anthropicUsageCacheReadTokens totals),
    "Output: " <> formatCount (anthropicUsageOutputTokens totals),
    "Window: " <> formatDuration (round $ diffUTCTime (anthropicUsageWindowEnd window) (anthropicUsageWindowStart window)),
    "Resets in: " <> formatDuration (max 0 $ round $ diffUTCTime (anthropicUsageWindowEnd window) generatedAt),
    "Display: " <> formatWindowValue displayMode window
  ]
    <> catMaybes
      [ ("Budget: " <>) . formatCount <$> anthropicUsageWindowBudgetTokens window,
        ("Estimated cost: " <>) . formatCurrency <$> anthropicUsageEstimatedCostUSD totals
      ]
  where
    totals = anthropicUsageWindowTotals window

formatSubscriptionStatus :: AnthropicUsageInfo -> Maybe T.Text
formatSubscriptionStatus info =
  case anthropicUsageHasAvailableSubscription info of
    Nothing -> Nothing
    Just True -> Just "Subscription: available"
    Just False -> Just "Subscription: unavailable"

formatWindow :: AnthropicUsageDisplayMode -> UTCTime -> AnthropicUsageWindow -> T.Text
formatWindow displayMode generatedAt window =
  anthropicUsageWindowName window
    <> " "
    <> formatWindowValue displayMode window
    <> ", "
    <> T.pack (show $ anthropicUsageRequestCount $ anthropicUsageWindowTotals window)
    <> " req"
    <> ", resets in "
    <> formatDuration (max 0 $ round $ diffUTCTime (anthropicUsageWindowEnd window) generatedAt)

formatWindowLabel :: AnthropicUsageDisplayMode -> AnthropicUsageWindow -> T.Text
formatWindowLabel displayMode window =
  anthropicUsageWindowName window
    <> " "
    <> formatWindowValueWithIndicator displayMode window

formatWindowValue :: AnthropicUsageDisplayMode -> AnthropicUsageWindow -> T.Text
formatWindowValue displayMode window =
  case anthropicUsageWindowUtilizationPercent window of
    Just utilization -> T.pack $ printf "%d%%" $ utilizationPercent displayMode utilization
    Nothing ->
      case anthropicUsageWindowBudgetTokens window of
        Nothing -> formatCount $ displayTokens displayMode window
        Just budget -> T.pack $ printf "%d%%" $ displayPercent displayMode budget window

utilizationPercent :: AnthropicUsageDisplayMode -> Double -> Int
utilizationPercent AnthropicUsageDisplayUsed utilization = round utilization
utilizationPercent AnthropicUsageDisplayRemaining utilization = max 0 (100 - round utilization)

formatWindowValueWithIndicator :: AnthropicUsageDisplayMode -> AnthropicUsageWindow -> T.Text
formatWindowValueWithIndicator displayMode window =
  formatWindowValue displayMode window <> displayModeIndicator displayMode

displayTokens :: AnthropicUsageDisplayMode -> AnthropicUsageWindow -> Int
displayTokens AnthropicUsageDisplayUsed = windowTotalTokens
displayTokens AnthropicUsageDisplayRemaining =
  \window ->
    case anthropicUsageWindowBudgetTokens window of
      Nothing -> windowTotalTokens window
      Just budget -> max 0 $ budget - windowTotalTokens window

displayPercent :: AnthropicUsageDisplayMode -> Int -> AnthropicUsageWindow -> Int
displayPercent displayMode budget window
  | budget <= 0 = 0
  | otherwise =
      let usedPercent = round $ (fromIntegral (windowTotalTokens window) / fromIntegral budget :: Double) * 100
       in case displayMode of
            AnthropicUsageDisplayUsed -> usedPercent
            AnthropicUsageDisplayRemaining -> max 0 (100 - usedPercent)

windowTotalTokens :: AnthropicUsageWindow -> Int
windowTotalTokens window =
  let totals = anthropicUsageWindowTotals window
   in anthropicUsageInputTokens totals
        + anthropicUsageCacheCreationTokens totals
        + anthropicUsageCacheReadTokens totals
        + anthropicUsageOutputTokens totals

formatTotalTokens :: AnthropicUsageTotals -> T.Text
formatTotalTokens totals =
  formatCount $
    anthropicUsageInputTokens totals
      + anthropicUsageCacheCreationTokens totals
      + anthropicUsageCacheReadTokens totals
      + anthropicUsageOutputTokens totals

displayModeText :: AnthropicUsageDisplayMode -> T.Text
displayModeText AnthropicUsageDisplayUsed = "used"
displayModeText AnthropicUsageDisplayRemaining = "remaining"

displayModeIndicator :: AnthropicUsageDisplayMode -> T.Text
displayModeIndicator AnthropicUsageDisplayUsed = "u"
displayModeIndicator AnthropicUsageDisplayRemaining = "r"

selectedWindow :: AnthropicUsageWindowSelector -> AnthropicUsageInfo -> Maybe AnthropicUsageWindow
selectedWindow AnthropicUsageFiveHourWindow = Just . anthropicUsageFiveHourWindow
selectedWindow AnthropicUsageWeeklyWindow = Just . anthropicUsageWeeklyWindow
selectedWindow AnthropicUsageScopedWeeklyWindow = anthropicUsageScopedWeeklyWindow

formatDuration :: Int -> T.Text
formatDuration seconds
  | seconds < 60 = T.pack $ printf "%ds" seconds
  | seconds < 3600 = T.pack $ printf "%dm" (seconds `div` 60)
  | seconds < 86400 = T.pack $ printf "%dh" (seconds `div` 3600)
  | otherwise = T.pack $ printf "%dd" (seconds `div` 86400)

formatCount :: Int -> T.Text
formatCount count
  | count >= 1_000_000 = T.pack $ printf "%.1fM" (fromIntegral count / 1_000_000 :: Double)
  | count >= 10_000 = T.pack $ printf "%dk" (count `div` 1_000)
  | count >= 1_000 = T.pack $ printf "%.1fk" (fromIntegral count / 1_000 :: Double)
  | otherwise = T.pack $ show count

formatCurrency :: Double -> T.Text
formatCurrency amount = T.pack $ printf "$%.2f" amount

module System.Taffybar.Widget.Text.NetworkMonitor where

import Control.Monad
import Control.Concurrent
import Control.Monad.Trans
import Graphics.UI.Gtk
import System.Taffybar.Context
import System.Taffybar.Hooks
import System.Taffybar.Information.Network
import Text.Printf
import Text.StringTemplate

defaultNetFormat :: String
defaultNetFormat = "▼ $inAuto$ ▲ $outAuto$"

showInfo :: String -> Int -> (Double, Double) -> String
showInfo template prec (incomingb, outgoingb) =
  let
    attribs = [ ("inB", show incomingb)
              , ("inKB", toKB prec incomingb)
              , ("inMB", toMB prec incomingb)
              , ("inAuto", toAuto prec incomingb)
              , ("outB", show outgoingb)
              , ("outKB", toKB prec outgoingb)
              , ("outMB", toMB prec outgoingb)
              , ("outAuto", toAuto prec outgoingb)
              ]
  in
    render . setManyAttrib attribs $ newSTMP template

toKB :: Int -> Double -> String
toKB prec = setDigits prec . (/1024)

toMB :: Int -> Double -> String
toMB prec = setDigits prec . (/ (1024 * 1024))

setDigits :: Int -> Double -> String
setDigits dig = printf format
    where format = "%." ++ show dig ++ "f"

toAuto :: Int -> Double -> String
toAuto prec value = printf "%.*f%s" p v unit
  where value' = max 0 value
        mag :: Int
        mag = if value' == 0 then 0 else max 0 $ min 4 $ floor $ logBase 1024 value'
        v = value' / 1024 ** fromIntegral mag
        unit = case mag of
          0 -> "B/s"
          1 -> "KiB/s"
          2 -> "MiB/s"
          3 -> "GiB/s"
          4 -> "TiB/s"
          _ -> "??B/s" -- unreachable
        p :: Int
        p = max 0 $ floor $ fromIntegral prec - logBase 10 v

networkMonitorNew :: String -> Maybe [String] -> TaffyIO Widget
networkMonitorNew template interfaces = do
  NetworkInfoChan chan <- getNetworkChan
  let filterFn = maybe (const True) (flip elem) interfaces
  liftIO $ do
    ourChan <- dupChan chan
    label <- labelNew (Nothing :: Maybe String)
    widgetShowAll label
    _ <- on label realize $ void $ forkIO $ forever $ do
        (up, down) <- sumSpeeds . map snd . filter (filterFn . fst) <$> readChan ourChan
        let labelString =
              showInfo template 3 (fromRational down, fromRational up)
        postGUIAsync $ labelSetMarkup label labelString
    return $ toWidget label

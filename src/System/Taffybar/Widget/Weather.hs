{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a simple textual weather widget that polls
-- NOAA for weather data.  To find your weather station, you can use
--
-- <https://www.weather.gov/tg/siteloc>
--
-- For example, Madison, WI is KMSN.
--
-- NOAA provides several pieces of information in each request; you can control
-- which pieces end up in your weather widget by providing a _template_ that is
-- filled in with the current information. The template is just a 'String' with
-- variables between dollar signs. The variables will be substituted with real
-- data by the widget. Example:
--
-- > let wcfg = (defaultWeatherConfig "KMSN") { weatherTemplate = "$tempC$ C @ $humidity$" }
-- >     weatherWidget = weatherNew wcfg 10
--
-- This example makes a new weather widget that checks the weather at KMSN
-- (Madison, WI) every 10 minutes, and displays the results in Celcius.
--
-- Available variables:
--
-- [@stationPlace@] The name of the weather station
--
-- [@stationState@] The state that the weather station is in
--
-- [@year@] The year the report was generated
--
-- [@month@] The month the report was generated
--
-- [@day@] The day the report was generated
--
-- [@hour@] The hour the report was generated
--
-- [@wind@] The direction and strength of the wind
--
-- [@visibility@] Description of current visibility conditions
--
-- [@skyCondition@] ?
--
-- [@tempC@] The temperature in Celsius
--
-- [@tempF@] The temperature in Farenheit
--
-- [@dewPoint@] The current dew point
--
-- [@humidity@] The current relative humidity
--
-- [@pressure@] The current pressure
--
--
-- As an example, a template like
--
-- > "$tempF$ °F"
--
-- would yield a widget displaying the temperature in Farenheit with a small
-- label after it.
--
-- Implementation Note: the weather data parsing code is taken from xmobar. This
-- version of the code makes direct HTTP requests instead of invoking a separate
-- cURL process.

module System.Taffybar.Widget.Weather
  ( WeatherConfig(..)
  , WeatherInfo(..)
  , WeatherFormatter(WeatherFormatter)
  , weatherNew
  , weatherCustomNew
  , defaultWeatherConfig
  ) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GI.Gtk
import GI.GLib(markupEscapeText)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Text.Parsec
import Text.Printf
import Text.StringTemplate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.Taffybar.Widget.Generic.PollingLabel

data WeatherInfo = WI
  { stationPlace :: String
  , stationState :: String
  , year :: String
  , month :: String
  , day :: String
  , hour :: String
  , wind :: String
  , visibility :: String
  , skyCondition :: String
  , tempC :: Int
  , tempF :: Int
  , dewPoint :: String
  , humidity :: Int
  , pressure :: Int
  } deriving (Show)

-- Parsers stolen from xmobar

type Parser = Parsec String ()

pTime :: Parser (String, String, String, String)
pTime = do
  y <- getNumbersAsString
  _ <- char '.'
  m <- getNumbersAsString
  _ <- char '.'
  d <- getNumbersAsString
  _ <- char ' '
  (h:hh:mi:mimi) <- getNumbersAsString
  _ <- char ' '
  return (y, m, d , [h]++[hh]++":"++[mi]++mimi)

pTemp :: Parser (Int, Int)
pTemp = do
  let num = digit <|> char '-' <|> char '.'
  f <- manyTill num $ char ' '
  _ <- manyTill anyChar $ char '('
  c <- manyTill num $ char ' '
  _ <- skipRestOfLine
  return (floor (read c :: Double), floor (read f :: Double))

pRh :: Parser Int
pRh = do
  s <- manyTill digit $ char '%' <|> char '.'
  return $ read s

pPressure :: Parser Int
pPressure = do
  _ <- manyTill anyChar $ char '('
  s <- manyTill digit $ char ' '
  _ <- skipRestOfLine
  return $ read s

parseData :: Parser WeatherInfo
parseData = do
  st <- getAllBut ","
  _ <- space
  ss <- getAllBut "("
  _ <- skipRestOfLine >> getAllBut "/"
  (y,m,d,h) <- pTime
  w <- getAfterString "Wind: "
  v <- getAfterString "Visibility: "
  sk <- getAfterString "Sky conditions: "
  _ <- skipTillString "Temperature: "
  (tC,tF) <- pTemp
  dp <- getAfterString "Dew Point: "
  _ <- skipTillString "Relative Humidity: "
  rh <- pRh
  _ <- skipTillString "Pressure (altimeter): "
  p <- pPressure
  _ <- manyTill skipRestOfLine eof
  return $ WI st ss y m d h w v sk tC tF dp rh p

getAllBut :: String -> Parser String
getAllBut s =
    manyTill (noneOf s) (char $ head s)

getAfterString :: String -> Parser String
getAfterString s = pAfter <|> return ("<" ++ s ++ " not found!>")
  where
    pAfter = do
      _ <- try $ manyTill skipRestOfLine $ string s
      manyTill anyChar newline

skipTillString :: String -> Parser String
skipTillString s =
    manyTill skipRestOfLine $ string s

getNumbersAsString :: Parser String
getNumbersAsString = skipMany space >> many1 digit >>= \n -> return n

skipRestOfLine :: Parser Char
skipRestOfLine = do
  _ <- many $ noneOf "\n\r"
  newline

-- | Simple: download the document at a URL.
downloadURL :: Manager -> Request -> IO (Either String String)
downloadURL mgr request = do
  response <- httpLbs request mgr
  case responseStatus response of
    s | s >= status200 && s < status300 ->
      return $ Right (T.unpack . T.decodeUtf8 . LB.toStrict $ responseBody response)
    otherStatus ->
      return . Left $ "HTTP 2XX status was expected but received " ++ show otherStatus

getWeather :: Manager -> String -> IO (Either String WeatherInfo)
getWeather mgr url = do
  request <- parseRequest url
  dat <- downloadURL mgr request
  case dat of
    Right dat' -> case parse parseData url dat' of
      Right d -> return (Right d)
      Left err -> return (Left (show err))
    Left err -> return (Left (show err))

defaultFormatter :: StringTemplate String -> WeatherInfo -> String
defaultFormatter tpl wi = render tpl'
  where
    tpl' = setManyAttrib [ ("stationPlace", stationPlace wi)
                         , ("stationState", stationState wi)
                         , ("year", year wi)
                         , ("month", month wi)
                         , ("day", day wi)
                         , ("hour", hour wi)
                         , ("wind", wind wi)
                         , ("visibility", visibility wi)
                         , ("skyCondition", skyCondition wi)
                         , ("tempC", show (tempC wi))
                         , ("tempF", show (tempF wi))
                         , ("dewPoint", dewPoint wi)
                         , ("humidity", show (humidity wi))
                         , ("pressure", show (pressure wi))
                         ] tpl

getCurrentWeather :: IO (Either String WeatherInfo)
    -> StringTemplate String
    -> StringTemplate String
    -> WeatherFormatter
    -> IO (T.Text, Maybe T.Text)
getCurrentWeather getter labelTpl tooltipTpl formatter = do
  dat <- getter
  case dat of
    Right wi ->
      case formatter of
        DefaultWeatherFormatter -> do
          let rawLabel = T.pack $ defaultFormatter labelTpl wi
          let rawTooltip = T.pack $ defaultFormatter tooltipTpl wi
          lbl <- markupEscapeText rawLabel (-1)
          tooltip <- markupEscapeText rawTooltip (-1)
          return (lbl, Just tooltip)
        WeatherFormatter f -> do
          let rawLabel = T.pack $ f wi
          lbl <- markupEscapeText rawLabel (-1)
          return (lbl, Just lbl)
    Left err -> do
      putStrLn err
      return ("N/A", Nothing)

-- | The NOAA URL to get data from
baseUrl :: String
baseUrl = "https://tgftp.nws.noaa.gov/data/observations/metar/decoded"

-- | A wrapper to allow users to specify a custom weather formatter.
-- The default interpolates variables into a string as described
-- above.  Custom formatters can do basically anything.
data WeatherFormatter
  = WeatherFormatter (WeatherInfo -> String) -- ^ Specify a custom formatter for 'WeatherInfo'
  | DefaultWeatherFormatter -- ^ Use the default StringTemplate formatter

-- | The configuration for the weather widget.  You can provide a custom
-- format string through 'weatherTemplate' as described above, or you can
-- provide a custom function to turn a 'WeatherInfo' into a String via the
-- 'weatherFormatter' field.
data WeatherConfig = WeatherConfig
  { weatherStation :: String -- ^ The weather station to poll. No default
  , weatherTemplate :: String -- ^ Template string, as described above.  Default: $tempF$ °F
  , weatherTemplateTooltip :: String -- ^ Template string, as described above.  Default: $tempF$ °F
  , weatherFormatter :: WeatherFormatter -- ^ Default: substitute in all interpolated variables (above)
  , weatherProxy :: Maybe String -- ^ The proxy server, e.g. "http://proxy:port". Default: Nothing
  }

-- | A sensible default configuration for the weather widget that just
-- renders the temperature.
defaultWeatherConfig :: String -> WeatherConfig
defaultWeatherConfig station =
  WeatherConfig
  { weatherStation = station
  , weatherTemplate = "$tempF$ °F"
  , weatherTemplateTooltip =
      unlines
        [ "Station: $stationPlace$"
        , "Time: $day$.$month$.$year$ $hour$"
        , "Temperature: $tempF$ °F"
        , "Pressure: $pressure$ hPa"
        , "Wind: $wind$"
        , "Visibility: $visibility$"
        , "Sky Condition: $skyCondition$"
        , "Dew Point: $dewPoint$"
        , "Humidity: $humidity$"
        ]
  , weatherFormatter = DefaultWeatherFormatter
  , weatherProxy = Nothing
  }

-- | Create a periodically-updating weather widget that polls NOAA.
weatherNew :: MonadIO m
           => WeatherConfig -- ^ Configuration to render
           -> Double     -- ^ Polling period in _minutes_
           -> m GI.Gtk.Widget
weatherNew cfg delayMinutes = liftIO $ do
  -- TODO: add explicit proxy host/port to WeatherConfig and
  -- get rid of this ugly stringly-typed setting
  let usedProxy = case weatherProxy cfg of
        Nothing -> noProxy
        Just str ->
          let strToBs = T.encodeUtf8 . T.pack
              noHttp = fromMaybe str $ stripPrefix "http://" str
              (phost, pport) = case span (':'/=) noHttp of
                (h, "") -> (strToBs h, 80) -- HTTP seems to assume 80 to be the default
                (h, ':':p) -> (strToBs h, read p)
                _ -> error "unreachable: broken span"
          in useProxy $ Proxy phost pport
  mgr <- newManager $ managerSetProxy usedProxy tlsManagerSettings
  let url = printf "%s/%s.TXT" baseUrl (weatherStation cfg)
  let getter = getWeather mgr url
  weatherCustomNew getter (weatherTemplate cfg) (weatherTemplateTooltip cfg)
    (weatherFormatter cfg) delayMinutes

-- | Create a periodically-updating weather widget using custom weather getter
weatherCustomNew
  :: MonadIO m
  => IO (Either String WeatherInfo) -- ^ Weather querying action
  -> String -- ^ Weather template
  -> String -- ^ Weather template
  -> WeatherFormatter -- ^ Weather formatter
  -> Double -- ^ Polling period in _minutes_
  -> m GI.Gtk.Widget
weatherCustomNew getter labelTpl tooltipTpl formatter delayMinutes = liftIO $ do
  let labelTpl' = newSTMP labelTpl
      tooltipTpl' = newSTMP tooltipTpl

  l <- pollingLabelNewWithTooltip (delayMinutes * 60)
       (getCurrentWeather getter labelTpl' tooltipTpl' formatter)

  GI.Gtk.widgetShowAll l
  return l

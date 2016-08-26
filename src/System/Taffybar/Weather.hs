-- | This module defines a simple textual weather widget that polls
-- NOAA for weather data.  To find your weather station, you can use
--
-- <http://www.nws.noaa.gov/tg/siteloc.php>
--
-- For example, Madison, WI is KMSN.
--
-- NOAA provides several pieces of information in each request; you
-- can control which pieces end up in your weather widget by providing
-- a _template_ that is filled in with the current information.  The
-- template is just a 'String' with variables between dollar signs.
-- The variables will be substituted with real data by the widget.
-- Example:
--
-- > let wcfg = (defaultWeatherConfig "KMSN") { weatherTemplate = "$tempC$ C @ $humidity$" }
-- >     weatherWidget = weatherNew wcfg 10
--
-- This example makes a new weather widget that checks the weather at
-- KMSN (Madison, WI) every 10 minutes, and displays the results in
-- Celcius.
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
-- [@tempC@] The temperature in Celcius
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
-- would yield a widget displaying the temperature in Farenheit with a
-- small label after it.
--
-- Implementation Note: the weather data parsing code is taken from
-- xmobar.  This version of the code makes direct HTTP requests
-- instead of invoking a separate cURL process.
module System.Taffybar.Weather (
  -- * Types
  WeatherConfig(..),
  WeatherInfo(..),
  WeatherFormatter(WeatherFormatter),
  -- * Constructor
  weatherNew,
  weatherCustomNew,
  defaultWeatherConfig
  ) where

import Network.HTTP
import Network.URI
import Graphics.UI.Gtk
import Text.Parsec
import Text.Printf
import Text.StringTemplate

import System.Taffybar.Widgets.PollingLabel

data WeatherInfo =
    WI { stationPlace :: String
       , stationState :: String
       , year         :: String
       , month        :: String
       , day          :: String
       , hour         :: String
       , wind         :: String
       , visibility   :: String
       , skyCondition :: String
       , tempC        :: Int
       , tempF        :: Int
       , dewPoint     :: String
       , humidity     :: Int
       , pressure     :: Int
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
  return (y, m, d ,([h]++[hh]++":"++[mi]++mimi))

pTemp :: Parser (Int, Int)
pTemp = do
  let num = digit <|> char '-' <|> char '.'
  f <- manyTill num $ char ' '
  _ <- manyTill anyChar $ char '('
  c <- manyTill num $ char ' '
  _ <- skipRestOfLine
  return $ (floor (read c :: Double), floor (read f :: Double))

pRh :: Parser Int
pRh = do
  s <- manyTill digit $ (char '%' <|> char '.')
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
      v <- manyTill anyChar $ newline
      return v

skipTillString :: String -> Parser String
skipTillString s =
    manyTill skipRestOfLine $ string s

getNumbersAsString :: Parser String
getNumbersAsString = skipMany space >> many1 digit >>= \n -> return n


skipRestOfLine :: Parser Char
skipRestOfLine = do
  _ <- many $ noneOf "\n\r"
  newline


-- | Simple: download the document at a URL.  Taken from Real World
-- Haskell.
downloadURL :: String -> IO (Either String String)
downloadURL url = do
  resp <- simpleHTTP request
  case resp of
    Left x -> return $ Left ("Error connecting: " ++ show x)
    Right r ->
      case rspCode r of
        (2,_,_) -> return $ Right (rspBody r)
        (3,_,_) -> -- A HTTP redirect
          case findHeader HdrLocation r of
            Nothing -> return $ Left (show r)
            Just url' -> downloadURL url'
        _ -> return $ Left (show r)
  where
    request = Request { rqURI = uri
                      , rqMethod = GET
                      , rqHeaders = []
                      , rqBody = ""
                      }
    Just uri = parseURI url

getWeather :: String -> IO (Either String WeatherInfo)
getWeather url = do
  dat <- downloadURL url
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
    -> WeatherFormatter
    -> IO String
getCurrentWeather getter tpl formatter = do
  dat <- getter
  case dat of
    Right wi -> do
      case formatter of
        DefaultWeatherFormatter -> return (defaultFormatter tpl wi)
        WeatherFormatter f -> return (f wi)
    Left err -> do
      putStrLn err
      return "N/A"

-- | The NOAA URL to get data from
baseUrl :: String
baseUrl = "http://tgftp.nws.noaa.gov/data/observations/metar/decoded"

-- | A wrapper to allow users to specify a custom weather formatter.
-- The default interpolates variables into a string as described
-- above.  Custom formatters can do basically anything.
data WeatherFormatter = WeatherFormatter (WeatherInfo -> String) -- ^ Specify a custom formatter for 'WeatherInfo'
                      | DefaultWeatherFormatter -- ^ Use the default StringTemplate formatter

-- | The configuration for the weather widget.  You can provide a custom
-- format string through 'weatherTemplate' as described above, or you can
-- provide a custom function to turn a 'WeatherInfo' into a String via the
-- 'weatherFormatter' field.
data WeatherConfig =
  WeatherConfig { weatherStation :: String   -- ^ The weather station to poll. No default
                , weatherTemplate :: String  -- ^ Template string, as described above.  Default: $tempF$ °F
                , weatherFormatter :: WeatherFormatter -- ^ Default: substitute in all interpolated variables (above)
                }

-- | A sensible default configuration for the weather widget that just
-- renders the temperature.
defaultWeatherConfig :: String -> WeatherConfig
defaultWeatherConfig station = WeatherConfig { weatherStation = station
                                             , weatherTemplate = "$tempF$ °F"
                                             , weatherFormatter = DefaultWeatherFormatter
                                             }

-- | Create a periodically-updating weather widget that polls NOAA.
weatherNew :: WeatherConfig -- ^ Configuration to render
              -> Double     -- ^ Polling period in _minutes_
              -> IO Widget
weatherNew cfg delayMinutes = do
  let url = printf "%s/%s.TXT" baseUrl (weatherStation cfg)
      getter = getWeather url
  weatherCustomNew getter (weatherTemplate cfg) (weatherFormatter cfg) delayMinutes

-- | Create a periodically-updating weather widget using custom weather getter
weatherCustomNew :: IO (Either String WeatherInfo) -- ^ Weather querying action
                 -> String                         -- ^ Weather template
                 -> WeatherFormatter               -- ^ Weather formatter
                 -> Double                         -- ^ Polling period in _minutes_
                 -> IO Widget
weatherCustomNew getter tpl formatter delayMinutes = do
  let tpl' = newSTMP tpl

  l <- pollingLabelNew "N/A" (delayMinutes * 60) (getCurrentWeather getter tpl' formatter)

  widgetShowAll l
  return l

module Main where

import Control.Concurrent.MVar
import Control.Monad
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative
import Paths_status_notifier_item (version)
import StatusNotifier.Item.Service
import Text.Printf

itemsParamsParser = ItemParams
  <$> strOption
  (  long "icon-name"
  <> short 'n'
  <> metavar "NAME"
  <> value "emacs"
  <> help "The icon the item will display."
  ) <*> strOption
  (  long "overlay-name"
  <> short 'o'
  <> metavar "NAME"
  <> value "steam"
  <> help "The icon that will be used for the overlay."
  ) <*> strOption
  (  long "dbus-name"
  <> short 'd'
  <> metavar "NAME"
  <> value "org.SampleSNI"
  <> help "The dbus name used for this sample item."
  )

versionOption :: Parser (a -> a)
versionOption = infoOption
                (printf "status-notifier-item %s" $ showVersion version)
                (  long "version"
                <> help "Show the version number of gtk-sni-tray"
                )

main :: IO ()
main = do
  itemParams <- execParser $ info (helper <*> versionOption <*> itemsParamsParser)
                (  fullDesc
                <> progDesc "Run a static StatusNotifierItem"
                )
  buildItem itemParams
  void $ getChar


{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Information.Nvidia
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- NVIDIA GPU information obtained from @nvidia-smi@.
module System.Taffybar.Information.Nvidia
  ( NvidiaGpuTemperature (..),
    parseNvidiaGpuTemperatures,
    readNvidiaGpuTemperatures,
    readNvidiaGpuTemperaturesWith,
  )
where

import Control.Exception (IOException, try)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | A temperature reported for one NVIDIA GPU.
data NvidiaGpuTemperature = NvidiaGpuTemperature
  { nvidiaGpuIndex :: Int,
    nvidiaGpuTemperatureCelsius :: Double
  }
  deriving (Eq, Show)

-- | Parse @index, temperature.gpu@ rows emitted by @nvidia-smi@.
-- Invalid rows, including unavailable temperature values, are ignored.
parseNvidiaGpuTemperatures :: T.Text -> [NvidiaGpuTemperature]
parseNvidiaGpuTemperatures =
  sortOn nvidiaGpuIndex . mapMaybe parseLine . T.lines
  where
    parseLine line =
      case map T.strip $ T.splitOn "," line of
        [indexText, temperatureText] ->
          NvidiaGpuTemperature
            <$> readMaybe (T.unpack indexText)
            <*> readMaybe (T.unpack temperatureText)
        _ -> Nothing

-- | Read temperatures using @nvidia-smi@ from @PATH@.
-- Returns an empty list when the command is missing or exits unsuccessfully.
readNvidiaGpuTemperatures :: IO [NvidiaGpuTemperature]
readNvidiaGpuTemperatures = readNvidiaGpuTemperaturesWith "nvidia-smi"

-- | Read temperatures using the supplied @nvidia-smi@ executable.
readNvidiaGpuTemperaturesWith :: FilePath -> IO [NvidiaGpuTemperature]
readNvidiaGpuTemperaturesWith command = do
  result <-
    try
      ( readProcessWithExitCode
          command
          [ "--query-gpu=index,temperature.gpu",
            "--format=csv,noheader,nounits"
          ]
          ""
      ) ::
      IO (Either IOException (ExitCode, String, String))
  pure $ case result of
    Right (ExitSuccess, output, _) -> parseNvidiaGpuTemperatures $ T.pack output
    _ -> []

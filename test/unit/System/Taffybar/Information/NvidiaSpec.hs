{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Information.NvidiaSpec (spec) where

import Data.Text qualified as T
import System.Taffybar.Information.Nvidia
import Test.Hspec

spec :: Spec
spec = do
  describe "NVIDIA information parsing" $ do
    it "parses rich nvidia-smi XML snapshots and optional readings" $ do
      let info = parseNvidiaGpuInfo sampleXml
      map nvidiaInfoIndex info `shouldBe` [0, 2]
      case info of
        firstGpu : _ -> do
          nvidiaInfoName firstGpu `shouldBe` "NVIDIA Test GPU"
          nvidiaInfoTemperatureCelsius firstGpu `shouldBe` Just 61
          nvidiaInfoMemoryTemperatureCelsius firstGpu `shouldBe` Nothing
          nvidiaInfoTargetTemperatureCelsius firstGpu `shouldBe` Just 87
          nvidiaInfoThermalHeadroomCelsius firstGpu `shouldBe` Just 26
          nvidiaInfoGpuUtilizationPercent firstGpu `shouldBe` Just 37
          nvidiaInfoMemoryUsedMiB firstGpu `shouldBe` Just 2048
          nvidiaInfoPowerDrawWatts firstGpu `shouldBe` Just 34.5
          nvidiaInfoPowerLimitWatts firstGpu `shouldBe` Just 80
        [] -> expectationFailure "expected parsed GPU information"

    it "rejects malformed XML" $
      parseNvidiaGpuInfo "not xml" `shouldBe` []

  describe "NVIDIA temperature parsing" $ do
    it "parses and sorts nvidia-smi temperature rows" $
      parseNvidiaGpuTemperatures "2, 73\n0, 56\n"
        `shouldBe` [ NvidiaGpuTemperature 0 56,
                     NvidiaGpuTemperature 2 73
                   ]

    it "ignores malformed and unavailable rows" $
      parseNvidiaGpuTemperatures "0, N/A\nbad row\n1, 64\n"
        `shouldBe` [NvidiaGpuTemperature 1 64]

sampleXml :: T.Text
sampleXml =
  T.unlines
    [ "<nvidia_smi_log>",
      "  <gpu>",
      "    <product_name>NVIDIA Second GPU</product_name>",
      "    <minor_number>2</minor_number>",
      "    <temperature><gpu_temp>55 C</gpu_temp></temperature>",
      "  </gpu>",
      "  <gpu>",
      "    <product_name>NVIDIA Test GPU</product_name>",
      "    <minor_number>0</minor_number>",
      "    <fan_speed>N/A</fan_speed>",
      "    <performance_state>P2</performance_state>",
      "    <utilization><gpu_util>37 %</gpu_util><memory_util>3 %</memory_util></utilization>",
      "    <fb_memory_usage><used>2048 MiB</used><total>12282 MiB</total></fb_memory_usage>",
      "    <temperature>",
      "      <gpu_temp>61 C</gpu_temp>",
      "      <gpu_temp_tlimit>26 C</gpu_temp_tlimit>",
      "      <gpu_target_temperature>87 C</gpu_target_temperature>",
      "      <memory_temp>N/A</memory_temp>",
      "    </temperature>",
      "    <gpu_power_readings>",
      "      <average_power_draw>34.50 W</average_power_draw>",
      "      <current_power_limit>80.00 W</current_power_limit>",
      "    </gpu_power_readings>",
      "  </gpu>",
      "</nvidia_smi_log>"
    ]

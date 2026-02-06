module System.Taffybar.Widget.HyprlandWorkspacesSpec where

import Test.Hspec

import qualified Data.Text as T

import System.Taffybar.Widget.HyprlandWorkspaces
  ( HyprlandWindow(..)
  , sortHyprlandWindowsByPosition
  )

mkWin :: String -> Bool -> Maybe (Int, Int) -> HyprlandWindow
mkWin addr minimized atPos =
  HyprlandWindow
    { windowAddress = T.pack addr
    , windowTitle = addr
    , windowClass = Nothing
    , windowInitialClass = Nothing
    , windowAt = atPos
    , windowUrgent = False
    , windowActive = False
    , windowMinimized = minimized
    }

spec :: Spec
spec = do
  describe "sortHyprlandWindowsByPosition" $ do
    it "orders non-minimized windows first, then by (x, y) position" $ do
      let a = mkWin "a" False (Just (10, 0))
          b = mkWin "b" False (Just (0, 100))
          c = mkWin "c" True  (Just (0, 0))
      sortHyprlandWindowsByPosition [a, b, c] `shouldBe` [b, a, c]

    it "puts windows without a position at the end" $ do
      let a = mkWin "a" False Nothing
          b = mkWin "b" False (Just (0, 0))
      sortHyprlandWindowsByPosition [a, b] `shouldBe` [b, a]


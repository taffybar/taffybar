import Data.Either (isLeft)
import Data.Foldable (for_)
import System.Environment.XDG.DesktopEntry
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

fileContent :: [String]
fileContent = [
    "[Desktop Entry]\n\
    \Icon=1",

    "[Desktop Entry]\n\
    \icon=2",

    "[desktop entry]\n\
    \Icon=3"
  ]

main :: IO ()
main = withSystemTempDirectory "xdg-desktop-entry" $ \dir -> do
  let filepath :: Int -> String
      filepath i = dir </> show i
  for_ (zip [0::Int ..] fileContent) $ \(i, content) -> do
    print i
    writeFile (filepath i) content
  hspec $ do
    describe "deAtt" $ do
      it "content0 should work" $ do
        deResultE <- readDesktopEntry $ filepath 0
        case deResultE of
          Left e -> expectationFailure $ show e
          Right deResult ->
            deIcon deResult `shouldBe` Just "1"
      it "content1 should not work" $ do
        deResultE <- readDesktopEntry $ filepath 1
        print deResultE
        case deResultE of
          Left e -> expectationFailure $ show e
          Right deResult ->
            deIcon deResult `shouldBe` Nothing
      it "content2 should not work" $ do
        deResultE <- readDesktopEntry $ filepath 2
        isLeft deResultE `shouldBe` True

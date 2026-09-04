import Data.Either (isLeft)
import Data.Foldable (for_)
import System.Environment.XDG.DesktopEntry
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

fileContent :: [String]
fileContent =
  [ "[Desktop Entry]\n\
    \Icon=1",
    "[Desktop Entry]\n\
    \icon=2",
    "[desktop entry]\n\
    \Icon=3",
    "# Localised keys, comments, padding around '=' and extra groups.\n\
    \[Desktop Entry]\n\
    \Version=1.0\n\
    \Name=Google Chrome\n\
    \# Only KDE 4 seems to use GenericName.\n\
    \GenericName=Web Browser\n\
    \GenericName[ar]=متصفح الشبكة\n\
    \Name[de]=Google Chrome (de)\n\
    \Icon = google-chrome\n\
    \Exec=/usr/bin/google-chrome %U\n\
    \Actions=new-window;\n\
    \\n\
    \[Desktop Action new-window]\n\
    \Name=New Window\n\
    \Exec=/usr/bin/google-chrome",
    "Icon=before-any-group\n\
    \[Desktop Entry]\n\
    \Icon=4",
    "[Desktop Entry]\n\
    \this line has no equals sign"
  ]

main :: IO ()
main = withSystemTempDirectory "xdg-desktop-entry" $ \dir -> do
  let filepath :: Int -> String
      filepath i = dir </> show i
  for_ (zip [0 :: Int ..] fileContent) $ \(i, content) -> do
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
    describe "readDesktopEntry" $ do
      it "parses localised keys, comments and extra groups" $ do
        deResultE <- readDesktopEntry $ filepath 3
        case deResultE of
          Left e -> expectationFailure $ show e
          Right deResult -> do
            deIcon deResult `shouldBe` Just "google-chrome"
            deName [] deResult `shouldBe` "Google Chrome"
            deName ["de"] deResult `shouldBe` "Google Chrome (de)"
            deName ["ar"] deResult `shouldBe` "Google Chrome"
            deCommand deResult `shouldBe` Just "/usr/bin/google-chrome"
            lookup "GenericName[ar]" (deAttributes deResult)
              `shouldBe` Just "متصفح الشبكة"
            lookup "Name" (deAttributes deResult) `shouldBe` Just "Google Chrome"
      it "rejects entries before any group header" $ do
        deResultE <- readDesktopEntry $ filepath 4
        isLeft deResultE `shouldBe` True
      it "rejects lines that are neither headers nor key=value" $ do
        deResultE <- readDesktopEntry $ filepath 5
        isLeft deResultE `shouldBe` True

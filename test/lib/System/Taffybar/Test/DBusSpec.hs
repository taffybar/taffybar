module System.Taffybar.Test.DBusSpec
  ( spec,

    -- * Start private D-Busses for testing
    withTestDBus,
    withTestDBusInDir,
    Bus (..),
    withDBusDaemon_,
    withConnectDBusDaemon,
    withConnectDBusDaemon',

    -- ** Using the private D-Bus
    setDBusEnv,
    withBusEnv,

    -- ** @python-dbusmock@ Services
    withPythonDBusMock,
    withTaffyMocks,

    -- * Utils
    withMatch,
    withClient,
  )
where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import DBus
import DBus.Client
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, (<.>), (</>))
import System.IO (Handle, hClose, hGetLine)
import System.Process.Typed
import System.Taffybar.Test.UtilSpec (getSpecLogPriority, laxTimeout', logSetup, setServiceDefaults, specLog, withService, withSetEnv)
import Test.Hspec
import UnliftIO.Directory (createDirectoryIfMissing, createFileLink, makeAbsolute)
import UnliftIO.Exception (bracket, finally, throwIO, throwString)
import UnliftIO.MVar qualified as MV
import UnliftIO.Temporary (withSystemTempDirectory)

-- | Uses 'withDBusDaemon_' to provide both a private session bus and
-- a private system bus while the given action is running.
--
-- The @DBUS_SESSION_BUS_ADDRESS@ and @DBUS_SYSTEM_BUS_ADDRESS@
-- environment variables will be set to point to socket files within
-- the given directory.
--
-- Files in the directory will be left behind after this function
-- returns.
--
-- __Note__: Environment variables are global to the process, so be
-- careful using this with 'parallel' unit tests.
withTestDBusInDir ::
  -- | Directory for config files and sockets.
  FilePath ->
  IO a ->
  IO a
withTestDBusInDir socketDir =
  withDBusDaemon_ System socketDir
    . withDBusDaemon_ Session socketDir

-- | Same as 'withTestDBusInDir', except that it creates and removes the
-- temporary directory for you.
withTestDBus :: IO a -> IO a
withTestDBus = withSystemTempDirectory "dbus-spec" . flip withTestDBusInDir

data Bus = Session | System deriving (Show, Read, Eq, Enum)

busName :: Bus -> String
busName Session = "session"
busName System = "system"

busArg :: Bus -> String
busArg = ("--" ++) . busName

envName :: Bus -> String
envName Session = "DBUS_SESSION_BUS_ADDRESS"
envName System = "DBUS_SYSTEM_BUS_ADDRESS"

busEnv :: Bus -> Address -> (String, String)
busEnv bus addr = (envName bus, formatAddress addr)

-- | Adjust a 'ProcessConfig' so that the child process will use the
-- given D-Bus address.
setDBusEnv :: Bus -> Address -> ProcessConfig i o e -> ProcessConfig i o e
setDBusEnv bus addr = setEnv [busEnv bus addr]

withDBusDaemon :: Bus -> FilePath -> (Address -> IO a) -> IO a
withDBusDaemon bus socketDir action = do
  cfg <- makeDBusDaemon <$> setupBusDir bus socketDir <*> getSpecLogPriority
  specLog $ "withDBusDaemon " ++ show bus ++ " running: " ++ show cfg
  withService cfg $ \p -> consumeAddress (getStdout p) >>= action
  where
    makeDBusDaemon configFile logLevel =
      proc "dbus-daemon" ["--print-address", "--config-file", configFile]
        & setServiceDefaults logLevel
        & setStdout createPipe

-- | Read and parse one dbus address from dbus-daemon's stdout.
consumeAddress :: Handle -> IO Address
consumeAddress h = (parse =<< hGetLine h) `finally` hClose h
  where
    parse s = maybe (throwString "Could not parse address from dbus-daemon") pure (parseAddress s)

-- | Start a D-Bus daemon of the given 'Bus' type, and set the
-- corresponding environment variable while running the given action.
--
-- __Note__: Environment variables are global to the process, so be
-- careful using this with 'parallel' unit tests. A safer option could
-- be 'withConnectDBusDaemon'' and 'setBusEnv'.
withDBusDaemon_ :: Bus -> FilePath -> IO a -> IO a
withDBusDaemon_ bus socketDir action = withDBusDaemon bus socketDir $ \addr -> withBusEnv bus addr action

-- | Same as 'withDBusDaemon', but it also provides a 'Client'
-- connection.
withConnectDBusDaemon' :: Bus -> FilePath -> (Address -> Client -> IO a) -> IO a
withConnectDBusDaemon' bus socketDir action =
  withDBusDaemon bus socketDir $ \addr ->
    withClient addr $ \c -> action addr c

withConnectDBusDaemon :: Bus -> FilePath -> (Client -> IO a) -> IO a
withConnectDBusDaemon bus socketDir = withConnectDBusDaemon' bus socketDir . const

setupBusDir :: Bus -> FilePath -> IO FilePath
setupBusDir bus socketDir = do
  let busDir = socketDir </> busName bus
      serviceDir = busDir </> "service.d"
      configFile = busDir </> "config.xml"
  createDirectoryIfMissing True serviceDir
  forM_ [] $ \service ->
    createFileLink service (serviceDir </> takeFileName service)
  -- createFileLink "/nix/store/ygd600kkc1h3p5dgw9vjm5xnfci43v0k-upower-1.90.4/share/dbus-1/system-services/org.freedesktop.UPower.service" (serviceDir </> "org.freedesktop.UPower.service")

  addr <- mkAddress (socketDir </> busName bus <.> "socket")
  writeFile configFile $
    unlines
      [ "<!DOCTYPE busconfig PUBLIC \"-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN\" \"http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd\">",
        "<busconfig>",
        "  <type>" ++ busName bus ++ "</type>",
        "  <keep_umask/>",
        "  <listen>" ++ addr ++ "</listen>",
        "  <servicedir>" ++ serviceDir ++ "</servicedir>",
        "  <policy context=\"default\">",
        "    <allow send_destination=\"*\" eavesdrop=\"true\"/>",
        "    <allow eavesdrop=\"true\"/>",
        "    <allow own=\"*\"/>",
        "  </policy>",
        "</busconfig>"
      ]
  pure configFile

mkAddress :: FilePath -> IO String
mkAddress = fmap ("unix:path=" ++) . makeAbsolute

-- | Set the @DBUS_SESSION_BUS_ADDRESS@ or @DBUS_SYSTEM_BUS_ADDRESS@
-- environment variable according to the given bus and address.
--
-- __Note 1__: Environment variables are global to the process, so
-- be careful using this with 'parallel' unit tests.
--
-- __Note 2__. Using a @DBUS_SYSTEM_BUS_ADDRESS@ environment variable to set a
-- custom system bus address is supported by libdbus (therefore
-- python-dbus) and haskell-dbus, but not necessarily other libraries
-- or programs. Notably, systemd hardcodes the system bus address.
withBusEnv :: Bus -> Address -> IO a -> IO a
withBusEnv bus addr = withSetEnv [busEnv bus addr]

withClient :: Address -> (Client -> IO a) -> IO a
withClient addr = bracket (connect addr) disconnect

withMatch :: (MonadUnliftIO m) => Client -> MatchRule -> (Signal -> m ()) -> m a -> m a
withMatch client rule cb action = withRunInIO $ \run ->
  bracket
    (addMatch client rule (run . cb))
    (removeMatch client)
    (const $ run action)

makeBusNameWaiter :: Client -> (BusName -> Bool) -> IO (IO ())
makeBusNameWaiter client p = do
  v <- MV.newEmptyMVar
  h <- MV.newEmptyMVar
  let cb sig = onNameOwnerChanged sig $ do
        hh <- MV.takeMVar h
        removeMatch client hh
        MV.putMVar v ()
  MV.putMVar h =<< addMatch client rule cb
  pure (MV.takeMVar v)
  where
    rule =
      matchAny
        { matchMember = Just "NameOwnerChanged",
          matchInterface = Just "org.freedesktop.DBus",
          matchSender = Just "org.freedesktop.DBus"
        }
    isMatch = maybe False p . fromVariant
    isOwned = not . null . fromMaybe ("" :: String) . fromVariant

    onNameOwnerChanged sig next = case signalBody sig of
      [name, _, owner] -> when (isMatch name && isOwned owner) next
      _ -> pure ()

-- | Starts up [@python-dbusmock@](https://martinpitt.github.io/python-dbusmock/).
-- The given action will be run once the mock is ready.
withPythonDBusMock ::
  -- | @python-dbusmock@ wants to know which bus.
  Bus ->
  -- | Connection to the 'Bus'
  (Address, Client) ->
  -- | Name of mock service.
  BusName ->
  -- | Path of mock service.
  ObjectPath ->
  -- | Interface of mock service.
  InterfaceName ->
  IO a ->
  IO a
withPythonDBusMock bus (addr, client) name path interface action = do
  waiter <- makeBusNameWaiter client (== name)
  logLevel <- getSpecLogPriority
  withService (cfg & setServiceDefaults logLevel) $
    const $
      waiter *> action
  where
    cfg = proc "python3" args & setDBusEnv bus addr
    args =
      [ "-m",
        "dbusmock",
        busArg bus,
        formatBusName name,
        formatObjectPath path,
        formatInterfaceName interface
      ]

mockAddTemplate :: Client -> BusName -> ObjectPath -> String -> [(String, Variant)] -> IO ()
mockAddTemplate client dest path templ params = do
  void $ call_ client (addTemplate templ params) {methodCallDestination = Just dest}
  where
    addTemplate t p =
      (methodCall path "org.freedesktop.DBus.Mock" "AddTemplate")
        { methodCallBody = [toVariant t, toVariant (Map.fromList p)]
        }

------------------------------------------------------------------------

upName :: BusName
upName = "org.freedesktop.UPower"

upPath, upDisplayDevicePath :: ObjectPath
upPath = "/org/freedesktop/UPower"
upDisplayDevicePath = objectPath_ (formatObjectPath upPath ++ "/devices/DisplayDevice")

upIface, upDeviceIface :: InterfaceName
upIface = "org.freedesktop.UPower"
upDeviceIface = interfaceName_ (formatInterfaceName upIface ++ ".Device")

mockIconName :: String
mockIconName = "face-cool-symbolic"

mockUPower :: Client -> IO ()
mockUPower client = do
  -- oh dbus, so ugly.
  mockAddTemplate client upName upPath "upower" [("OnBattery", toVariant True)]
  void $ call_ client (methodCall upPath "org.freedesktop.DBus.Mock" "AddAC") {methodCallBody = map toVariant ["mock_AC" :: String, "Mock AC"], methodCallDestination = Just upName}
  void $ call_ client (methodCall upPath "org.freedesktop.DBus.Mock" "AddChargingBattery") {methodCallBody = map toVariant ["mock_BAT" :: String, "Mock Battery"] ++ [toVariant (30.0 :: Double), toVariant (1200 :: Int64)], methodCallDestination = Just upName}
  void $ setPropertyValue client (methodCall upDisplayDevicePath upDeviceIface "IconName") {methodCallDestination = Just upName} mockIconName

withTaffyMocks :: IO a -> IO a
withTaffyMocks action = do
  maddr <- getSystemAddress
  addr <- maybe (throwIO (clientError "getSystemAddress")) pure maddr
  withClient addr $ \client -> do
    withPythonDBusMock System (addr, client) upName upPath upIface $ do
      mockUPower client
      action

------------------------------------------------------------------------

spec :: Spec
spec = logSetup $ around_ (laxTimeout' 1_000_000) $ around (withSystemTempDirectory "dbus-spec") $ do
  describe "withDBusDaemon org.freedesktop.DBus.Peer.Ping" $ do
    forM_ [System, Session] $ \bus ->
      aroundWith (flip (withConnectDBusDaemon' bus) . curry) $ do
        it ("can ping private test " ++ show bus ++ " bus") $ \(_, client) -> do
          (fmap methodReturnBody <$> call client ping)
            `shouldReturn` Right []

        it ("gdbus can ping private test " ++ show bus ++ " bus") $ \(addr, _) ->
          readProcessStdout_ (gdbusPing bus & setDBusEnv bus addr)
            `shouldReturn` "()\n"

  forM_ [System] $ \bus ->
    aroundWith (flip (withConnectDBusDaemon' bus) . curry) $
      describe ("python-dbusmock " ++ show bus ++ " services") $ do
        -- These tests are currently failing in CI due to python-dbusmock startup issues.
        -- Mark as pending until the root cause is identified.
        it "simple" $ \_ -> pendingWith "python-dbusmock fails to start in CI"

        it "UPower" $ \_ -> pendingWith "python-dbusmock fails to start in CI"

  describe "dbus-daemon restarts" $
    it "existing clients do not recover automatically after daemon restart" $ \socketDir -> do
      configFile <- setupBusDir Session socketDir
      logLevel <- getSpecLogPriority
      let cfg =
            proc "dbus-daemon" ["--print-address", "--config-file", configFile]
              & setServiceDefaults logLevel
              & setStdout createPipe

      withProcessTerm cfg $ \p1 -> do
        addr1 <- consumeAddress (getStdout p1)
        withClient addr1 $ \client1 -> do
          (fmap methodReturnBody <$> call client1 ping) `shouldReturn` Right []

          stopProcess p1
          _ <- waitExitCode p1

          resultAfterStop <- laxTimeout' 1_000_000 (call client1 ping)
          resultAfterStop `shouldSatisfy` isLeft

          withProcessTerm cfg $ \p2 -> do
            addr2 <- consumeAddress (getStdout p2)
            addr2 `shouldBe` addr1

            withClient addr2 $ \client2 -> do
              (fmap methodReturnBody <$> call client2 ping) `shouldReturn` Right []

            oldClientResult <- laxTimeout' 1_000_000 (call client1 ping)
            oldClientResult `shouldSatisfy` isLeft

gdbusPing :: Bus -> ProcessConfig () () ()
gdbusPing bus = proc "gdbus" ["call", "--" ++ busName bus, "--dest", "org.freedesktop.DBus", "--object-path", "/org/freedesktop/DBus", "--method", "org.freedesktop.DBus.Peer.Ping"]

ping :: MethodCall
ping =
  (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus.Peer" "Ping")
    { methodCallDestination = Just "org.freedesktop.DBus"
    }

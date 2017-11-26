module Installer.Server where

import Installer.UI
import Installer.Progress
import Installer.Types
import Installer.StoreUserInput
import Installer.Target
import Propellor hiding (createProcess, waitForProcess)
import Propellor.Location
import Propellor.Types.CmdLine
import qualified Propellor.Property.File as File
import qualified Propellor.Property.FreeDesktop as FreeDesktop
import qualified Propellor.Property.Systemd as Systemd

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import System.FilePath
import System.Process
import System.Environment
import System.Exit
import System.IO

installerServer :: IO ()
installerServer = do
	userinput <- newEmptyTMVarIO
	progress <- newTChanIO
	targetdev <- probeDisk
	_ <- serveUI localdir targetdev userinput progress
		`concurrently` awaitConfirmationAndInstall userinput progress
	return ()

awaitConfirmationAndInstall :: TMVar UserInput -> TChan Progress -> IO ()
awaitConfirmationAndInstall userinputv progresschan = do
	userinput <- atomically $ takeTMVar userinputv
	storeUserInput userinput
	-- Run propellor to ensure the properties of the installer,
	-- which includes a property that installs the target system.
	-- This is run in a separate process both to allow it to be run
	-- independently of the UI for debugging, and so its output can be
	-- captured.
	-- TODO this displays to stdout, but this is running as a service,
	-- so that will go in the journal. Need some kind of activity
	-- display for the user, particularly when install somehow fails..
	let p = proc (localdir </> "propellor") 
			["--serialized", show (SimpleRun "debian.local")]
	environ <- getEnvironment
	let environ' = environ ++
		[ ("HOME", "/root")
		, ("LANG", "C.UTF-8")
		, ("PROPELLOR_TRACE", "1")
		]
	(Nothing, Just hout, Just herr, ph) <- createProcess $ p
			{ cwd = Just localdir
			, std_out = CreatePipe
			, std_err = CreatePipe
			, env = Just environ'
			}
	outfeeder <- async $ feed hout Out stdout
	errfeeder <- async $ feed herr Err stderr
	pp <- async $ progresspoller mempty =<< prepTargetFilled
	exitcode <- waitForProcess ph
	_ <- wait outfeeder
	_ <- wait errfeeder
	cancel pp
	atomically $ writeTChan progresschan $ ProgressDone exitcode
	case exitcode of
		-- We're done! Reboot into installed system.
		ExitSuccess -> callProcess "shutdown" ["-h", "now"]
		-- Keep running so the web browser can display the
		-- problem to the user.
		ExitFailure _ -> forever $ threadDelay 100000
  where
	feed fromh from toh = do
		ls <- parseOutput from <$> hGetContents fromh
		forM_ ls $ \l -> do
			let sendui = atomically $ writeTChan progresschan l
			case l of
				Line _ s -> do
					hPutStrLn toh s
					sendui
				ReplaceLine _ -> sendui
				_ -> do
					print l
					sendui
			atomically $ writeTChan progresschan l
	progresspoller oldtf h = do
		tf <- checkTargetFilled h
		when (tf /= oldtf) $
			atomically $ writeTChan progresschan $
				TargetFilledUpdate tf
		threadDelay 1000000
		progresspoller tf h

installerServerArg :: String
installerServerArg = "--installer-server"

-- | Installs a FreeDesktop autostart file that runs a web browser
-- to interact with the installer, and a systemd service that runs
-- the installer's UI server.
autostartInstaller :: RevertableProperty Linux Linux
autostartInstaller = fdoautostart 
	`requires` (serviceenabled <!> servicedisabled)
	`requires` (serviceinstalled <!> serviceremoved)
	`describe` "autostart installer UI"
  where
	fdoautostart = FreeDesktop.autostart
		(FreeDesktop.desktopFile "installer")
		"Installer"
		"firefox http://127.0.0.1:8023/"
	serviceenabled = Systemd.enabled servicename
	servicedisabled = Systemd.disabled servicename
	serviceinstalled = servicefile `File.hasContent` 
		[ "[Unit]"
		, "Description=propellor installer server"
		, ""
		, "[Service]"
		, "ExecStart=" ++ localdir ++ "/propellor " ++ installerServerArg
		, ""
		, "[Install]"
		, "WantedBy=multi-user.target"
		]
	serviceremoved = File.notPresent servicefile
	servicename = "propellor-installer-server"
	servicefile = "/lib/systemd/system/" ++ servicename ++ ".service"

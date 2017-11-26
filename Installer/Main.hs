module Installer.Main where

import Propellor
import Installer.Server
import qualified Installer.UI

import System.Environment (getArgs)

-- | For use in config.hs instead of propellor's usual defaultMain.
--
-- This adds a command-line argument which runs the installer 
-- GUI's web server. Otherwise the propellor command line is handled
-- as usual.
installerMain :: [Host] -> IO ()
installerMain hostlist = do
	args <- getArgs
	case args of
		[s]
			| s == installerServerArg -> installerServer
			| s == "--test-ui" -> Installer.UI.main
		_ -> defaultMain hostlist

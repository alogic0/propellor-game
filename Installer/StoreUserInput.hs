module Installer.StoreUserInput where

import Installer.Types
import Propellor.Location

import System.FilePath
import System.Process

-- | Update the Installer.UserInput module to contain the specified UserInput.
storeUserInput :: UserInput -> IO ()
storeUserInput userinput = do
	writeFile userInputModule $ unlines modulecontent
	ph <- spawnProcess "git"
		[ "commit"
		, "-m", "installer user input"
		, userInputModule
		]
	-- If git commit fails for some reason, that should not cause any
	-- problem for the installation, so ignore.
	_ <- waitForProcess ph
	return ()
  where
	modulecontent =
		[ "-- | User input at installation time."
		, "module Installer.UserInput (userInput) where"
		, ""
		, "import Installer.Types"
		, ""
		, "userInput :: UserInput"
		, "userInput = " ++ show userinput
		]

userInputModule :: FilePath
userInputModule = localdir </> "Installer" </> "UserInput.hs"

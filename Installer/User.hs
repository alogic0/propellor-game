module Installer.User where

import Propellor hiding (UserName)
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.XFCE as XFCE
import qualified Propellor.Property.LightDM as LightDM
import Installer.Types

import System.Directory

-- | Makes a desktop user. Lightdm automatically logs in as this user,
-- and the user has access to desktop groups and sudo.
--
-- Reverting this property deletes the user account, so use caution!
desktopUser :: Maybe UserName -> RevertableProperty DebianLike DebianLike
desktopUser musername = (setup <!> nuke)
	`describe` ("has desktop user " ++ username)
  where
	username = case musername of
		Just (UserName u) -> u
		Nothing -> case defaultUserName of
			UserName u -> u
	user = User username
	setup = User.accountFor user
		`before` User.hasDesktopGroups user
		`before` Sudo.enabledFor user
		`before` User.hasInsecurePassword user defaultPassword
		`before` LightDM.autoLogin user
		`before` noScreenLocking
	-- Deleting the user will remove it from the desktop groups,
	-- but also have to remove it from lightdm and sudo configs.
	nuke = User.nuked user User.YesReallyDeleteHome
		`requires` revert (Sudo.enabledFor user)
		`requires` revert (LightDM.autoLogin user)

-- | Make a user account that is available during installation.
installerUser :: RevertableProperty DebianLike DebianLike
installerUser = desktopUser (Just (UserName u)) `before` defaultdesktop
  where
	-- Avoid XFCE prompting about panel setup on initial login.
	defaultdesktop :: RevertableProperty DebianLike DebianLike
	defaultdesktop = XFCE.defaultPanelFor (User u) File.OverwriteExisting
		-- No need to handle reversion here, since reverting
		-- desktopUser deletes all the files.
		<!> doNothing
	u = "installer"

-- | Disable light-locker, because it would prompt for the default
-- password, which the user may not know.
noScreenLocking :: RevertableProperty UnixLike UnixLike
noScreenLocking = setup <!> cleanup
  where
	f = "/etc/xdg/autostart/light-locker.desktop"
	df = f ++ ".disabled"
  	setup = check (doesFileExist f) $
		cmdProperty "mv" [f, df] `assume` MadeChange
	cleanup = check (not <$> doesFileExist f) $
		cmdProperty "mv" [df, f] `assume` MadeChange

-- | Default user account.
defaultUserName :: UserName
defaultUserName = UserName "user"

-- | Default password used for installer etc until the user changes it.
--
-- Of course, this is only secure if the system does not run anything
-- that allows remote access via password.
defaultPassword :: String
defaultPassword = "debian"

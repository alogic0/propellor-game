module Installer.Types where

newtype TargetDiskDevice = TargetDiskDevice FilePath
	deriving (Read, Show)

data DiskEraseConfirmed = DiskEraseConfirmed
	deriving (Read, Show)

newtype UserName = UserName String
	deriving (Read, Show)

-- | This is serialized out to the Installer.UserInput module,
-- so should not contain passwords or other sensative information.
-- (Store such information in propellor PrivData instead.)
data UserInput = UserInput
	{ targetDiskDevice :: Maybe TargetDiskDevice
	, diskEraseConfirmed :: Maybe DiskEraseConfirmed
	, inputUserName :: Maybe UserName
	}
	deriving (Read, Show)

noUserInput :: UserInput
noUserInput = UserInput
	{ targetDiskDevice = Nothing
	, diskEraseConfirmed = Nothing
	, inputUserName = Nothing
	}

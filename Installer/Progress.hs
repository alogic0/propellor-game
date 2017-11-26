module Installer.Progress where

import Propellor.Message
import Installer.Target (TargetFilled)

import System.Exit

data Progress
	= Line Src String
	| ReplaceLine String
	| Progress Trace
	| TargetFilledUpdate TargetFilled
	| ProgressDone ExitCode
	deriving (Show)

data Src = Out | Err
	deriving (Show)

-- | Parses propellor's output.
parseOutput :: Src -> String -> [Progress]
parseOutput src = go []
  where
	go l [] = [Line src (reverse l)]
	go l (c:cs)
		| c == '\n' =
			let ln = reverse l
			in case parseTrace ln of
				Nothing -> Line src ln : go [] cs
				Just t -> Progress t : go [] cs
		| c == '\r' = Line src (reverse l) : replaceline [] cs
		| otherwise = go (c:l) cs
	replaceline l [] = [ReplaceLine (reverse l)]
	replaceline l (c:cs)
		| c == '\n' = ReplaceLine (reverse l) : go [] cs
		| c == '\r' = ReplaceLine (reverse l) : replaceline [] cs
		| otherwise = replaceline (c:l) cs

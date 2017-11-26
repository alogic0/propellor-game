{- Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Installer.UI (main, serveUI) where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import Graphics.UI.Threepenny.Core
import Control.Monad
import Data.Ratio
import Data.IORef
import System.Posix.Directory
import System.Random
import System.Random.Shuffle
import System.Exit
import Data.Function ((&))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import Installer.Types
import Installer.User
import Installer.Progress
import Installer.Target (TargetFilledPercent(..), TargetFilled(..), targetFilledPercent)
import Propellor.Message
import Propellor.Types.Result

-- | This module can be run as a standalone program, using this main.
-- Useful for testing without building a disk image.
--
-- This reads ./log, which should be a log of a propellor run.
main :: IO ()
main = do
	v <- newEmptyTMVarIO
	p <- newTChanIO
	feeder <- async $ logfeeder p . parseOutput Out =<< readFile "log"
	pp <- async $ dummyprogresspoller p 0
	let targetdev = TargetDiskDevice "/dev/null"
	serveUI "." targetdev v p
	wait feeder
	cancel pp
  where
	logfeeder p [] = atomically $ writeTChan p $ ProgressDone ExitSuccess
	logfeeder p (l:ls) = do
		threadDelay 100000
		atomically $ writeTChan p l
		logfeeder p ls
	dummyprogresspoller p n = do
		atomically $ writeTChan p $ 
			TargetFilledUpdate $ TargetFilled (n%100)
		threadDelay 1000000
		dummyprogresspoller p (succ n)		

-- | Serve the UI. Pass the directory that contains the "static"
-- subdirectory.
--
-- The TMVar UserInput will be populated once the user confirms that the
-- installation should proceed.
serveUI :: FilePath -> TargetDiskDevice -> TMVar UserInput -> TChan Progress -> IO ()
serveUI dir targetdev result progresschan = do
	-- Work around for
	-- https://github.com/HeinrichApfelmus/threepenny-gui/issues/194
	changeWorkingDirectory dir
	startGUI defaultConfig { jsStatic = Just staticdir } $
		treeegg targetdev result progresschan
  where
	staticdir = "static"

treeegg :: TargetDiskDevice -> TMVar UserInput -> TChan Progress -> Window -> UI ()
treeegg targetdev result progresschan window = do
	_ <- return window # set title "Installation"
	UI.addStyleSheet window "fullscreen.css"

	startupSpinner window

	dropSuccess <- liftIO $ newIORef (False, (0,0))
	bg <- UI.div #. "fullscreen" #+ [svgBackground]
	allowDropOn dropSuccess bg

	clock <- mkClock

	shuffledcruft <- liftIO $ shuffleM cruft
	cruftcount <- liftIO $ mkCruftCount (length shuffledcruft)
	cruftelts <- mapM (mkCruft clock cruftcount dropSuccess)
		(zip shuffledcruft (cycle [1,2,3]))
	
	let onceontree = do
		_ <- getBody window 
			& set children []
		liftIO $ atomically $ putTMVar result $ UserInput
			{ targetDiskDevice = Just targetdev
			, diskEraseConfirmed = Just DiskEraseConfirmed
			, inputUserName = Just defaultUserName
			}
		progressdisplay progresschan window

	egg <- mkEgg clock cruftcount dropSuccess onceontree
	_ <- getBody window &+ map element ([bg] ++ cruftelts ++ [egg])
	return ()

progressdisplay :: TChan Progress -> Window -> UI ()
progressdisplay progresschan window = do
	-- Screen is split into top and bottom divs, with a progress bar
	-- above both.
	progressbar <- UI.div
		& set UI.style
			( progresscommon ++
			[ ("width", "0%")
			, ("background", debianred)
			, ("left", "0")
			, ("top", "0")
			, ("position", "absolute")
			, ("text-align", "right")
			])
	progress <- UI.div
		&+ [element progressbar]
		& set UI.style
			( progresscommon ++
			[ ("overflow", "hidden")
			, ("position", "relative")
			, ("font-size", "4em")
			])
	let updateprogressbar (TargetFilledPercent p)
		| p < 1 = return ()
		| p >= 100 = delete progressbar
		| otherwise = do
			let percent = show p ++ "%"
			_ <- element progressbar
				& set UI.style [("width", percent)]
				& set UI.text percent
			flushCallBuffer
	top <- UI.div
	bottom <- UI.div
	_ <- getBody window &+ [element progress, element top, element bottom]
	progressdisplay' progresschan window top bottom updateprogressbar
  where
	progresscommon = [ ("height", "1em") ]
	debianred = "#cc0066"

progressdisplay' :: TChan Progress -> Window -> UI.Element -> UI.Element -> (TargetFilledPercent -> UI ()) -> UI ()
progressdisplay' progresschan window top bottom updateprogressbar = startline
  where
	go spinner recenterrs lastline = do
		p <- liftIO $ atomically $ readTChan progresschan
		case p of
			-- Show current action at the top.
			Progress (ActionStart _ desc) -> do
				_ <- element top
					& set UI.text (desc ++ " ...")
				go spinner recenterrs lastline
			-- Only show failed actions to avoid cluttering
			-- the display.
			Progress (ActionEnd _ desc result) ->
				case result of
					FailedChange -> do
						replaceline lastline $ unlines $ 
							reverse recenterrs ++
							[desc ++ " ... failed!"]
						startline
					_ -> go spinner [] lastline
			Line Err l -> go spinner (l:recenterrs) lastline
			-- Regular output is not shown to the user
			-- to avoid cluttering the display. But do spin the
			-- spinner.
			Line Out _ -> do
				spin spinner
				go spinner recenterrs lastline
			-- A line being replaced is some kind of progress
			-- display, so show it.
			ReplaceLine l -> do
				replaceline lastline l
				go spinner recenterrs lastline
			TargetFilledUpdate tf -> do
				updateprogressbar $ targetFilledPercent tf
				spin spinner
				go spinner recenterrs lastline
			ProgressDone ExitSuccess ->
				spinForever window ".oOo."
			ProgressDone (ExitFailure _) -> 
				spinForever window "_ "

	startline = do
		(d, spinner) <- mkSpinner "/|\\-"
		_ <- element bottom &+ [element d]
		flushCallBuffer
		go spinner [] d

	replaceline d l = do
		_ <- element d & set UI.text l
		flushCallBuffer

type ObjName = String

cruft :: [ObjName]
cruft =
	[ "boot"
	, "pie"
	, "banana"
	, "capacitors"
	, "inchworm"
	, "filebox"
	, "garlic"
	, "gears"
	, "milkcarton"
	, "old-bicycle"
	, "scissors"
	, "spork"
	, "umbrella"
	]

type CruftCount = IORef Int

mkCruftCount :: Int -> IO CruftCount
mkCruftCount = newIORef

decrementCruftCount :: CruftCount -> IO ()
decrementCruftCount cc = atomicModifyIORef' cc (\n -> (pred n, ()))

cruftGone :: CruftCount -> IO Bool
cruftGone cc = do
	n <- readIORef cc
	return (n < 1)

loadSVG :: ObjName -> UI Element
loadSVG objname = do
	f <- UI.loadFile "image/svg+xml" ("static/" ++ objname ++ ".svg")
	UI.img & set UI.src f

mkIsland :: UI Element
mkIsland = do
	screenheight <- getScreenHeight
	screenwidth <- getScreenWidth
	loadSVG "Girl-Reading-Book-Under-A-Brain-Tree"
		& set UI.height screenheight
		& set UI.width (screenwidth `div` 6 * 5)
		& moveto (screenwidth `div` 12, screenheight `div` 24)

data Clock = Clock (Behavior Time)
type Time = Float

mkClock :: UI Clock
mkClock = do
	time <- UI.timer & set UI.interval 10
	UI.start time
	Clock <$> (accumB 0 $ succ <$ UI.tick time)

getTime :: Clock -> UI Time
getTime (Clock clock) = liftIO $ currentValue clock

mkMoveable :: ObjName -> (Float, Float) -> Float -> UI (Element, IORef (Int, Int))
mkMoveable objname (relx, rely) relwidth = do
	screenheight <- getScreenHeight
	screenwidth <- getScreenWidth
	let fracwidth n = floor (fromIntegral screenwidth * n)
	let fracheight n = floor (fromIntegral screenheight * n)
	let width = fracwidth relwidth
	let startx = fracwidth relx
	let starty = fracheight rely

	elt <- loadSVG objname
		& set UI.draggable True
		& set UI.dragData objname
		& set UI.width width
		& moveto (startx, starty)
	currpos <- liftIO $ newIORef (startx, starty)
	return (elt, currpos)

data MoveInfo = MoveInfo
	{ dropHandler :: (IORef [UI.Timer] -> (Int, Int) -> (Float, Float) -> UI ()) -> UI ()
	, movementTimers :: IORef [UI.Timer]
	}

type DropSuccess = (Bool, (Int, Int))

mkMoveInfo :: IORef DropSuccess -> Clock -> Element -> IORef (Int, Int) -> UI MoveInfo
mkMoveInfo dropSuccess clock elt currpos = do
	movementtimers <- liftIO $ newIORef []
	draginfo <- liftIO $ newIORef (0,0,0)

	on UI.dragStart elt $ \(_d, (x,y)) -> void $ do
		now <- getTime clock
		liftIO $ writeIORef draginfo (x,y,now)
		element elt 
			& set UI.style [("display", "none")]

	let drophandler a = do
		(dragstartx, dragstarty, dragstarttime) <- liftIO $
			readIORef draginfo
		dropped <- liftIO $ readIORef dropSuccess
		case dropped of
			(True, (x,y)) -> do
				now <- getTime clock
				let dt = now - dragstarttime
				(oldx, oldy) <- liftIO $ readIORef currpos
				let newx = x - (dragstartx - oldx)
				let newy = y - (dragstarty - oldy)
				liftIO $ writeIORef currpos (newx, newy)
				_ <- element elt
					& moveto (newx, newy)
					& set UI.style [("display", "block")]
				let dx = fromIntegral (x - dragstartx) / dt
				let dy = fromIntegral (min 10 $ max (-2) (y - dragstarty)) / dt
				a movementtimers (newx, newy) (dx, dy)
			_ -> return ()

	return (MoveInfo drophandler movementtimers)

mkEgg :: Clock -> CruftCount -> IORef DropSuccess -> UI () -> UI Element
mkEgg clock cruftcount dropSuccess onceontree = do
	(elt, currpos) <- mkMoveable "egg" (0.350, 0.650) 0.092
	(startx, starty) <- liftIO $ readIORef currpos
	let tostart = dropBackOnScreen elt (startx, starty)
	tostart
	
	moveinfo <- mkMoveInfo dropSuccess clock elt currpos
	on UI.dragEnd elt $ \_ ->
		dropHandler moveinfo $ dropEgg elt cruftcount onceontree $ do
			tostart
			liftIO $ writeIORef currpos (startx, starty)

	allowDropOn dropSuccess elt

	return elt

dropEgg :: Element -> CruftCount -> UI () -> UI () -> IORef [UI.Timer] -> (Int, Int) -> (Float, Float) -> UI ()
dropEgg elt cruftcount onceontree onceoffscreen movementtimers (x, y) (dx, dy) = do
	treebare <- liftIO $ cruftGone cruftcount
	overtree <- checkOverTree elt (x,y)
	if treebare && overtree
		then moveCenterTree elt (x, y) $
			spinGrow elt onceontree
		else moveOffScreen elt onceoffscreen movementtimers (x, y) (dx, dy)

mkCruft :: Clock -> CruftCount -> IORef DropSuccess -> (ObjName, Int) -> UI Element
mkCruft clock cruftcount dropSuccess (objname, objrow) = do
	relwidth <- liftIO $ getStdRandom $ randomR (0.073, 0.092)
	relx <- liftIO $ getStdRandom $ randomR (treeMinX, treeMaxX)
	let rely = treeMinY + fromIntegral objrow * (treeMaxY - treeMinY) / 3
	(elt, currpos) <- mkMoveable objname (relx, rely) relwidth 
	
	let dropaway = moveOffScreen elt $ do
		liftIO $ decrementCruftCount cruftcount
		_ <- element elt
			& set style [("display", "none")]
		delete elt

	moveinfo <- mkMoveInfo dropSuccess clock elt currpos
	on UI.dragEnd elt $ \_ -> dropHandler moveinfo dropaway
	on UI.click elt $ \_ -> do
		(x, y) <- liftIO $ readIORef currpos
		dropaway (movementTimers moveinfo) (x,y) (0,0)

	allowDropOn dropSuccess elt

	return elt

getScreenWidth :: UI Int
getScreenWidth = callFunction $ ffi "window.innerWidth"

getScreenHeight :: UI Int
getScreenHeight = callFunction $ ffi "window.innerHeight"

getObjectWidth :: Element -> UI Int
getObjectWidth el = callFunction $ ffi "%1.offsetWidth" el

getObjectHeight :: Element -> UI Int
getObjectHeight el = callFunction $ ffi "%1.offsetHeight" el

moveOffScreen :: Element -> UI () -> IORef [UI.Timer] -> (Int, Int) -> (Float, Float) -> UI ()
moveOffScreen elt onceoffscreen movementtimers (x, y) (dx, dy) = do
	mapM_ UI.stop =<< liftIO (readIORef movementtimers)

	screenwidth <- getScreenWidth
	screenheight <- getScreenHeight
	mywidth <- getObjectWidth elt
	myheight <- getObjectHeight elt

	timer <- UI.timer & set UI.interval 5
	liftIO $ writeIORef movementtimers [timer]

	tickctr <- accumE (1 :: Float) $ succ <$ UI.tick timer
	_ <- onEvent tickctr $ \n -> do
		let x' = x + floor (dx * 10 * (n / 40))
		let n' = n/5
		let y' = y + floor (n' * dy * 30 + (1/2*1.01*n'*n'))
		let offscreenx = x' <= 0 || x' >= screenwidth - mywidth - 10
		let offscreeny = y' >= screenheight - myheight - 10
		if offscreenx || offscreeny
			then do
				UI.stop timer
				onceoffscreen		
			else void $ element elt & moveto (x', y')
		flushCallBuffer
	UI.start timer

dropBackOnScreen :: Element -> (Int, Int) -> UI ()
dropBackOnScreen elt (destx, desty) = do
	_ <- element elt & set UI.style [("display", "none")]
	timer <- UI.timer & set UI.interval 5
	tickctr <- accumE (0 :: Float) $ succ <$ UI.tick timer
	_ <- onEvent tickctr $ \n -> do
		if n > dropdelay
			then do
				_ <- element elt & set UI.style [("display", "block")]
				let n' = (n-dropdelay)/5
				let y = floor (n' * 10 + (1/2*1.01*n'*n'))
				if y >= desty
					then do
						void $ element elt & moveto (destx, desty)
						UI.stop timer
					else void $ element elt & moveto (destx, y)
				flushCallBuffer
			else return ()
	UI.start timer
  where
	dropdelay = 150

checkOverTree :: Element -> (Int, Int) -> UI Bool
checkOverTree elt (x,y) = do
	screenheight <- getScreenHeight
	screenwidth <- getScreenWidth
	width <- getObjectWidth elt
	let fracwidth n = fromIntegral n / fromIntegral screenwidth
	let fracheight n = fromIntegral n / fromIntegral screenheight
	return $ and 
		[ fracwidth (x + width) >= treeMinX
		, fracwidth (x - width) <= treeMaxX
		, fracheight y <= treeMaxY
		]

moveCenterTree :: Element -> (Int, Int) -> (Float -> UI ()) -> UI ()
moveCenterTree elt (startx, starty) cont = do
	screenheight <- getScreenHeight
	screenwidth <- getScreenWidth
	let fracwidth n = floor (fromIntegral screenwidth * n)
	let fracheight n = floor (fromIntegral screenheight * n)
	let destx = fracwidth $ (treeMinX + treeMaxX) / 2
	let desty = fracheight $ (treeMinY + treeMaxY) / 2
	let movetoward start end n =
		let delta = fromIntegral (abs (start - end)) / 50
		in floor $ if start > end 
			then fromIntegral start - n*delta
			else fromIntegral start + n*delta
	let clamp start end n
		| start > end && n <= end = end
		| start < end && n >= end = end
		| otherwise = n

	timer <- UI.timer & set UI.interval 5
	tickctr <- accumE (0 :: Float) $ succ <$ UI.tick timer
	_ <- onEvent tickctr $ \n -> do
		let x = clamp startx destx $ movetoward startx destx n
		let y = clamp starty desty $ movetoward starty desty n
		if x == destx && y == desty
			then do
				void $ element elt & moveto (destx, desty)
				UI.stop timer
				cont n
			else void $ element elt 
				& moveto (x, y)
				& set UI.style
					[ ("transform", "rotate("++show n++ "deg)") ]
		flushCallBuffer
	UI.start timer

spinGrow :: Element -> UI () -> Float -> UI ()
spinGrow elt cont startrotation = do
	timer <- UI.timer & set UI.interval 5
	tickctr <- accumE (0 :: Float) $ succ <$ UI.tick timer
	_ <- onEvent tickctr $ \n -> do
		let rotatestyle = "rotate(" ++ show (startrotation + n*2) ++ "deg)"
		let scalestyle = "scale("++show (max 1 (2**(n/100)-10))++ ")"
		_ <- element elt & set UI.style
			[ ("transform", unwords [rotatestyle, scalestyle]) ]
		flushCallBuffer
		if n > 500
			then do
				UI.stop timer
				cont
			else return ()
	UI.start timer

allowDropOn :: IORef (Bool, (Int, Int)) -> Element -> UI ()
allowDropOn dropSuccess elt = do
	on UI.drop elt $ \(d, (x,y)) -> when (not (null d)) $
		liftIO $ writeIORef dropSuccess (True, (x,y))
	_ <- element elt & set UI.droppable True
	return ()

-- | Background, svg, scaled to occupy entire browser window.
svgBackground :: UI Element
svgBackground = do
	context <- SVG.svg
	    	& set SVG.x "0px"
		& set SVG.y "0px"
	        & set SVG.width "100%"
	        & set SVG.height "100%"
		& set SVG.preserveAspectRatio "xMidYMid meet"
		& set SVG.viewBox "0 0 1021 1566"
	elemSquare <- SVG.rect
	        & set SVG.width "100%"
	        & set SVG.height "100%"
	        & set SVG.fill "skyblue"
	-- Embedding the island in the background like this simplifies
	-- click and drag, since it's not treated as a separate object.
	island <- SVG.foreignObject
		&+ [mkIsland]
	    	& set SVG.x "0"
		& set SVG.y "0"
		& set SVG.width "100%"
		& set SVG.height "100%"
	return context &+ [element elemSquare, element island]

treeMinX, treeMaxX, treeMinY, treeMaxY :: Float
(treeMinX, treeMaxX, treeMinY, treeMaxY) = (0.403, 0.586, 0.037, 0.262)

moveto :: (Int, Int) -> UI Element -> UI Element
moveto (x, y) elt = elt # set style
	[ ("position", "absolute")
	, ("left", show x ++ "px")
	, ("top", show y ++ "px")
	]

(&+) :: UI Element -> [UI Element] -> UI Element
(&+) = (#+)

newtype Spinner = Spinner (UI ())

mkSpinner :: [Char] -> UI (UI.Element, Spinner)
mkSpinner cs = do
	d <- UI.div
	v <- liftIO $ newTVarIO $ concat $ repeat cs
	return (d, Spinner (go d v))
  where
	go d v = do
		c <- liftIO $ atomically $ do
			(c:cs') <- readTVar v
			writeTVar v cs'
			return c
		_ <- element d & set UI.text [c]
		flushCallBuffer

spin :: Spinner -> UI ()
spin (Spinner a) = a

spinForever :: Window -> [Char] -> UI ()
spinForever window cs = do
	(elt, spinner) <- mkSpinner cs
	_ <- getBody window &+ [element elt]
	forever $ do
		spin spinner
		liftIO $ threadDelay 100000

-- Display a spinner briefly on startup, mostly in case the browser window
-- gets resized as the desktop is starting up.
startupSpinner :: Window -> UI ()
startupSpinner window = do
	(d, spinner) <- mkSpinner ".oOo."
	_ <- getBody window &+ [element d]
	forM_ [(1 :: Int)..10] $ \_ -> do
		spin spinner
		liftIO $ threadDelay 100000
	delete d

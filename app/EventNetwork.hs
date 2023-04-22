module EventNetwork (run) where

import Control.Concurrent

import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as F

import Player
import Event

fps, delta, maxFps, maxDelta :: Double
fps = 30
delta = 1000 / fps
maxFps = 60
maxDelta = 1000 / maxFps

run :: IO [Event] -> IO Double -> (Player -> IO ()) -> IO ()
run getEvents ticks drawPlayer = do
	(eventHandler, fireEvent) <- F.newAddHandler

	network <- B.compile $ description eventHandler ticks drawPlayer
	F.actuate network

	handleEvents fireEvent 0 []

	where
		handleEvents :: F.Handler Event -> Double -> [Event] -> IO ()
		handleEvents fireEvent previousTick (QuitEvent:xs) = pure ()
		handleEvents fireEvent previousTick xs = do
			nextEvents <- case xs of
				[] -> getEvents
				(x:xs) -> fireEvent x *> pure xs
			now <- ticks
			handleNextEvents fireEvent previousTick now nextEvents
		handleNextEvents :: F.Handler Event -> Double -> Double -> [Event] -> IO ()
		handleNextEvents fireEvent previousTick now nextEvents 
			| now - previousTick > delta = do
				fireEvent $ TickEvent $ (now - previousTick) / delta
				handleEvents fireEvent now nextEvents
			| otherwise = do
				preventCpu100
				handleEvents fireEvent previousTick nextEvents

preventCpu100 :: IO ()
preventCpu100 = threadDelay (round maxDelta * 1000)

description :: F.AddHandler Event -> IO Double -> (Player -> IO ()) -> F.MomentIO ()
description eventHandler ticks drawPlayer = do
	events <- F.fromAddHandler eventHandler
	pressedKeysE <- B.accumE initialPressedKeys (updatePressedKeys <$> events)
	let
		tickE = B.filterE onlyTicks pressedKeysE
	playerE <- B.accumE initialPlayer (updatePlayer <$> tickE)
	F.reactimate $ drawPlayer <$> playerE
	where
		onlyTicks :: PressedKeys -> Bool
		onlyTicks pk = pk.executeTick

updatePressedKeys :: Event -> (PressedKeys -> PressedKeys)
updatePressedKeys (ArrowUpEvent b) k = k { up = b, executeTick = False }
updatePressedKeys (ArrowLeftEvent b) k = k { left = b, executeTick = False }
updatePressedKeys (ArrowRightEvent b) k = k { right = b, executeTick = False }
updatePressedKeys (TickEvent deltaLastTick) k = k { deltaLastTick = deltaLastTick, executeTick = True }
updatePressedKeys _ k = k

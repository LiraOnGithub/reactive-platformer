module Sdl (withSdl) where

import qualified SDL as S
import Data.StateVar (($=))
import Data.Maybe (mapMaybe)
import Linear.V2 (V2(V2))
import Linear.V4 (V4(V4))
import Linear.Affine (Point(P))
import Control.Monad ((<$!>))
import Control.Exception (bracket, bracket_)

import Player
import Event

withSdl :: (IO [Event] -> IO Double -> (Player -> IO ()) -> IO ()) -> IO ()
withSdl eventNetwork = bracket_
	(S.initialize [S.InitVideo])
	S.quit
	(withWindow $ withRenderer $ eventNetwork getSdlEvents ticks . draw)

draw :: S.Renderer -> Player -> IO ()
draw renderer player = do
	clear renderer
	drawPlayer renderer player
	S.present renderer

clear :: S.Renderer -> IO ()
clear renderer = do
	S.rendererDrawColor renderer $= V4 0 0 0 255
	S.clear renderer

drawPlayer :: S.Renderer -> Player -> IO ()
drawPlayer renderer player = do
	S.rendererDrawColor renderer $= if player.groundState == Grounded
		then V4 255 0 0 255
		else V4 0 255 0 255
	drawRectangle renderer (V2 player.x player.y) (V2 player.width player.height)

drawRectangle :: S.Renderer -> V2 Double -> V2 Double -> IO ()
drawRectangle renderer position size = S.fillRect renderer (toRectangle position size)

toRectangle :: Integral n => V2 Double -> V2 Double -> Maybe (S.Rectangle n)
toRectangle position size = Just $ S.Rectangle (P $ round <$> position) (round <$> size)

withWindow :: (S.Window -> IO ()) -> IO ()
withWindow f = bracket createWindow S.destroyWindow f
	where
		createWindow :: IO S.Window
		createWindow = do
			w <- S.createWindow "game" S.defaultWindow
			S.showWindow w
			pure w

withRenderer :: (S.Renderer -> IO ()) -> S.Window -> IO ()
withRenderer f w = bracket (S.createRenderer w (-1) S.defaultRenderer) S.destroyRenderer f

getSdlEvents :: IO [Event]
getSdlEvents = mapMaybe (mapEvent . S.eventPayload) <$!> S.pollEvents

mapEvent :: S.EventPayload -> Maybe Event
mapEvent S.QuitEvent = Just QuitEvent
mapEvent (S.KeyboardEvent (S.KeyboardEventData _ pressedStatus False (S.Keysym _ keyCode _)))
	| keyCode == S.KeycodeUp = Just $ ArrowUpEvent pressed
	| keyCode == S.KeycodeRight = Just $ ArrowRightEvent pressed
	| keyCode == S.KeycodeLeft = Just $ ArrowLeftEvent pressed
	| otherwise = Nothing
	where 
		pressed :: Bool
		pressed = pressedStatus == S.Pressed
mapEvent _ = Nothing

ticks :: IO Double
ticks = fromIntegral <$> S.ticks

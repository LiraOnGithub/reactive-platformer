module Sdl (withSdl) where

import qualified SDL
import qualified SDL.Image
import Data.StateVar (($=))
import Data.Maybe (mapMaybe)
import Data.Map (Map, fromList, (!))
import Linear.V2 (V2(V2))
import Linear.V4 (V4(V4))
import Linear.Affine (Point(P))
import Control.Monad ((<$!>))
import Control.Exception (bracket, bracket_)

import Sprite
import Player
import Event

withSdl :: (IO [Event] -> IO Double -> (Player -> IO ()) -> IO ()) -> IO ()
withSdl eventNetwork = bracket_
	(SDL.initialize [SDL.InitVideo] *> SDL.Image.initialize [SDL.Image.InitPNG])
	(SDL.Image.quit *> SDL.quit)
	(withWindow $ withRenderer $ withSprites $ \r t -> eventNetwork getSdlEvents ticks (draw r t))

withSprites :: (SDL.Renderer -> Map Sprite SDL.Texture -> IO ()) -> SDL.Renderer -> IO ()
withSprites f renderer = bracket
	loadSprites
	(mapM_ SDL.destroyTexture)
	(f renderer)
	where
		loadSprites :: IO (Map Sprite SDL.Texture)
		loadSprites = do
			player <- SDL.Image.loadTexture renderer "app/images/player_sprite.png"
			pure $ fromList [(PlayerSprite, player)]

draw :: SDL.Renderer -> Map Sprite SDL.Texture -> Player -> IO ()
draw renderer sprites player = do
	clear renderer
	drawPlayer renderer sprites player
	SDL.present renderer

clear :: SDL.Renderer -> IO ()
clear renderer = do
	SDL.rendererDrawColor renderer $= V4 0 0 0 255
	SDL.clear renderer

drawPlayer :: SDL.Renderer -> Map Sprite SDL.Texture -> Player -> IO ()
drawPlayer renderer sprites player =
	drawSprite renderer playerSprite player.spriteInformation.spriteAction player.spriteInformation.spriteIndex (V2 player.x player.y) (V2 player.spriteInformation.width player.spriteInformation.height)
	where
		playerSprite :: SDL.Texture
		playerSprite = sprites ! player.spriteInformation.sprite

drawSprite :: SDL.Renderer -> SDL.Texture -> SpriteAction -> Int-> V2 Double -> V2 Int -> IO ()
drawSprite renderer texture action index position size@(V2 width height) = SDL.copy renderer texture sourceRectangle targetRectangle
	where
		sourceRectangle :: Integral n => Maybe (SDL.Rectangle n)
		sourceRectangle = toRectangle (V2 (index * width) (fromEnum action * height)) (V2 width height)
		targetRectangle :: Integral n => Maybe (SDL.Rectangle n)
		targetRectangle = toRectangle (round <$> position) size


drawRectangle :: SDL.Renderer -> V2 Int -> V2 Int -> IO ()
drawRectangle renderer position size = SDL.fillRect renderer (toRectangle position size)

toRectangle :: (Integral n) => V2 Int -> V2 Int -> Maybe (SDL.Rectangle n)
toRectangle position size = Just $ SDL.Rectangle (P $ fromIntegral <$> position) (fromIntegral <$> size)

withWindow :: (SDL.Window -> IO ()) -> IO ()
withWindow f = bracket createWindow SDL.destroyWindow f
	where
		createWindow :: IO SDL.Window
		createWindow = do
			w <- SDL.createWindow "game" SDL.defaultWindow
			SDL.showWindow w
			pure w

withRenderer :: (SDL.Renderer -> IO ()) -> SDL.Window -> IO ()
withRenderer f w = bracket (SDL.createRenderer w (-1) SDL.defaultRenderer) SDL.destroyRenderer f

getSdlEvents :: IO [Event]
getSdlEvents = mapMaybe (mapEvent . SDL.eventPayload) <$!> SDL.pollEvents

mapEvent :: SDL.EventPayload -> Maybe Event
mapEvent SDL.QuitEvent = Just QuitEvent
mapEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ pressedStatus False (SDL.Keysym _ keyCode _)))
	| keyCode == SDL.KeycodeUp = Just $ ArrowUpEvent pressed
	| keyCode == SDL.KeycodeRight = Just $ ArrowRightEvent pressed
	| keyCode == SDL.KeycodeLeft = Just $ ArrowLeftEvent pressed
	| otherwise = Nothing
	where 
		pressed :: Bool
		pressed = pressedStatus == SDL.Pressed
mapEvent _ = Nothing

ticks :: IO Double
ticks = fromIntegral <$> SDL.ticks

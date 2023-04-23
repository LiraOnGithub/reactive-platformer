module Sprite where

import Vector

data Sprite
	= PlayerSprite
	| BrickSprite
	deriving (Show, Eq, Ord)

data SpriteAction
	= SpriteActionIdle
	| SpriteActionJump
	| SpriteActionFall
	| SpriteActionWalkLeft
	| SpriteActionWalkRight
 deriving (Show, Eq, Ord, Enum)

data SpriteInformation = SpriteInformation
	{ sprite :: Sprite
	, previousAction :: SpriteAction
	, action :: SpriteAction
	, index :: Int
	, repeats :: Bool
	, height :: Int
	, width :: Int
	, counter :: Int
	}
	deriving Show

spriteSpeed :: Int
spriteSpeed = 10

frameCount :: Sprite -> SpriteAction -> Int
frameCount PlayerSprite SpriteActionWalkLeft = 4
frameCount PlayerSprite SpriteActionWalkRight = 4
frameCount BrickSprite SpriteActionIdle = 4
frameCount _ _ = 1

setPreviousSpriteAction :: SpriteInformation -> SpriteInformation
setPreviousSpriteAction info = info { previousAction = info.action }

setSpriteIndex :: SpriteInformation -> SpriteInformation
setSpriteIndex info
	| info.previousAction /= info.action = info { index = 0, counter = spriteSpeed }
	| info.counter > 0 = info { counter = info.counter - 1 }
	| info.repeats = info { counter = spriteSpeed, index = (info.index + 1) `mod` frameCount info.sprite info.action } 
	| otherwise = info { counter = spriteSpeed, index = min (info.index + 1) (frameCount info.sprite info.action - 1) }

class HasSprite a where
	getSpriteToDraw :: a -> (Vec2 Int, SpriteInformation)

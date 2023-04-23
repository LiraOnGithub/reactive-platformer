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
	}
	deriving Show

frameCount :: Sprite -> SpriteAction -> Int
frameCount PlayerSprite SpriteActionWalkLeft = 4
frameCount PlayerSprite SpriteActionWalkRight = 4
frameCount BrickSprite SpriteActionIdle = 4
frameCount _ _ = 1

setPreviousSprite :: SpriteInformation -> SpriteInformation
setPreviousSprite info = info { previousAction = info.action }

setSpriteIndex :: SpriteInformation -> SpriteInformation
setSpriteIndex info
	| info.previousAction /= info.action = info { index = 0 }
	| info.repeats = info { index = (info.index + 1) `mod` frameCount info.sprite info.action } 
	| otherwise = info { index = min (info.index + 1) (frameCount info.sprite info.action - 1) }

class HasSprite a where
	getSpriteToDraw :: a -> (Vec2 Int, SpriteInformation)

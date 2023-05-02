{-# LANGUAGE DataKinds, FlexibleContexts  #-}
module Sprite where

import Vector
import GHC.Records (HasField)

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
	| info.previousAction /= info.action = info { counter = spriteSpeed, index = 0 }
	| info.counter > 0 = info { counter = info.counter - 1 }
	| info.repeats = info { counter = spriteSpeed, index = (info.index + 1) `mod` frameCount' }
	| otherwise = info { counter = spriteSpeed, index = min (info.index + 1) (frameCount' - 1) }
	where
		frameCount' :: Int
		frameCount' = frameCount info.sprite info.action

class (HasField "spriteInformation" a SpriteInformation) => HasSprite a where
	getSpritePosition :: a -> Vec2 Int
	getSpriteToDraw :: a -> (Vec2 Int, SpriteInformation)
	getSpriteToDraw a = (getSpritePosition a, a.spriteInformation)
	setSprite :: SpriteInformation -> a -> a

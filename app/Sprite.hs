module Sprite where

data Sprite
	= PlayerSprite
	deriving (Show, Eq, Ord)

data SpriteAction
	= SpriteActionIdle
	| SpriteActionJump
	| SpriteActionFall
	| SpriteActionWalkLeft
	| SpriteActionWalkRight
 deriving (Show, Eq, Ord, Enum)

frameCount :: Sprite -> SpriteAction -> Int
frameCount PlayerSprite SpriteActionIdle = 1
frameCount PlayerSprite SpriteActionJump = 1
frameCount PlayerSprite SpriteActionFall = 1
frameCount PlayerSprite SpriteActionWalkLeft = 4
frameCount PlayerSprite SpriteActionWalkRight = 4

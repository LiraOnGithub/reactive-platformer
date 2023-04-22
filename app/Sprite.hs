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

data SpriteInformation = SpriteInformation
	{ sprite :: Sprite
	, previousSpriteAction :: SpriteAction
	, spriteAction :: SpriteAction
	, spriteIndex :: Int
	, height :: Int
	, width :: Int
	}
	deriving Show

frameCount :: Sprite -> SpriteAction -> Int
frameCount PlayerSprite SpriteActionIdle = 1
frameCount PlayerSprite SpriteActionJump = 1
frameCount PlayerSprite SpriteActionFall = 1
frameCount PlayerSprite SpriteActionWalkLeft = 4
frameCount PlayerSprite SpriteActionWalkRight = 4

setPreviousSprite :: SpriteInformation -> SpriteInformation
setPreviousSprite player = player { previousSpriteAction = player.spriteAction }

setSpriteIndex :: SpriteInformation -> SpriteInformation
setSpriteIndex player
	| player.previousSpriteAction /= player.spriteAction = player { spriteIndex = 0 }
	| otherwise = player { spriteIndex = (player.spriteIndex + 1) `mod` frameCount player.sprite player.spriteAction } 

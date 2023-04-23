module Brick where

import Sprite
import Vector

data Brick = Brick
	{ position :: Vec2 Int

	, spriteInformation :: SpriteInformation
	}
	deriving Show

instance HasSprite Brick where
	getSpriteToDraw brick = (brick.position, brick.spriteInformation)

placeBrick :: Int -> Int -> Brick
placeBrick x y = Brick
	{ position = Vec2 x y
	, spriteInformation = SpriteInformation
		{ sprite = BrickSprite
		, previousAction = SpriteActionIdle
		, action = SpriteActionIdle
		, index = 0
		, width = 20
		, height = 20
		, repeats = True
		}
	}

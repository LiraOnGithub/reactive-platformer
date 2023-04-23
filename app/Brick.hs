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


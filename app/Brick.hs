module Brick where

import Sprite
import Vector

data Brick = Brick
	{ position :: Vec2 Double

	, spriteInformation :: SpriteInformation
	}
	deriving Show

instance HasSprite Brick where
	getSpriteToDraw brick = (round <$> brick.position, brick.spriteInformation)


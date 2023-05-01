module Brick where

import Sprite
import Vector
import Common.HasDefault

data Brick = Brick
	{ position :: Vec2 Double

	, spriteInformation :: SpriteInformation
	} deriving Show

instance HasSprite Brick where
	getSpritePosition brick = round <$> brick.position

instance HasDefault Brick where
	getDefault = Brick
		{ position = Vec2 0 0
		, spriteInformation = SpriteInformation
			{ sprite = BrickSprite
			, previousAction = SpriteActionIdle
			, action = SpriteActionIdle
			, index = 0
			, width = 20
			, height = 20
			, repeats = True
			, counter = 0
			}
		}

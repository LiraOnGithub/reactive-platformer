module Brick (brick, Brick(position, spriteInformation)) where

import Sprite
import Vector
import Common.HasDefault

data Brick = Brick
	{ position :: Vec2 Double

	, spriteInformation :: SpriteInformation
	} deriving Show

instance HasSprite Brick where
	getSpritePosition brick = round <$> brick.position
	setSprite spriteInformation brick = brick { spriteInformation = spriteInformation }

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

brick :: Vec2 Double -> Brick
brick v = getDefault { position = v }


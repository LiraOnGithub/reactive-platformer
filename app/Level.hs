module Level where

import Brick
import Player
import Sprite
import Vector

data Level = Level 
	{ bricks :: [Brick]
	, player :: Player
	}

initialLevel :: Level
initialLevel = Level
	{ bricks = [placeBrick 50 50, placeBrick 50 70]
	, player = initialPlayer
	}

spritesToDraw :: Level -> [(Vec2 Int, SpriteInformation)]
spritesToDraw level = (getSpriteToDraw <$> level.bricks) <> [(getSpriteToDraw level.player)]

module Level where

import Brick
import Player
import Sprite
import Vector
import Event
import Collision.LevelGrid
import Common.HasDefault

data Level = Level
	{ bricks :: LevelGrid Brick
	, player :: Player
	}

initialLevel :: Level
initialLevel = Level
	{ player = getDefault
	, bricks = createLevelGrid 10 20 [brick $ Vec2 0 180, brick $ Vec2 20 180, brick $ Vec2 40 180]
	}

spritesToDraw :: Level -> [(Vec2 Int, SpriteInformation)]
spritesToDraw level = (getSpriteToDraw <$> (getElements level.bricks)) <> [(getSpriteToDraw level.player)]

updateLevel :: PressedKeys -> (Level -> Level)
updateLevel pressedKeys level = updateSprites . handleCollisionDetection $ level
	{ player = updatePlayer pressedKeys level.player
	}

updateSprites :: (Level -> Level)
updateSprites level = level
	{ player = setSprite (setSpriteIndex level.player.spriteInformation) level.player
	, bricks = (\b -> setSprite (setSpriteIndex b.spriteInformation) b) <$> level.bricks
	}

handleCollisionDetection :: (Level -> Level)
handleCollisionDetection level = level
	{ player = handleCollisions level.bricks level.player
	}

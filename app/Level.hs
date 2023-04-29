module Level where

import qualified Brick
import qualified Player
import Sprite
import Vector
import Event
import LevelGrid

data Level = Level 
	{ bricks :: LevelGrid Brick.Brick
	, player :: Player.Player
	}

initialLevel :: Level
initialLevel = Level
	{ player = Player.initialPlayer
	, bricks = createLevelGrid 10 20 [placeBrick 0 180, placeBrick 20 180, placeBrick 40 180]
	}

spritesToDraw :: Level -> [(Vec2 Int, SpriteInformation)]
spritesToDraw level = (getSpriteToDraw <$> (getElements level.bricks)) <> [(getSpriteToDraw level.player)]

updateLevel :: PressedKeys -> (Level -> Level)
updateLevel pressedKeys level = level
	{ player = updatedPlayer { Player.spriteInformation = setSpriteIndex updatedPlayer.spriteInformation }
	, bricks = (\b -> b { Brick.spriteInformation = setSpriteIndex b.spriteInformation }) <$> level.bricks
	}
	where
		updatedPlayer :: Player.Player
		updatedPlayer = Player.updatePlayer pressedKeys level.player

placeBrick :: Double -> Double -> Brick.Brick
placeBrick x y = Brick.Brick
	{ position = Vec2 x y
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

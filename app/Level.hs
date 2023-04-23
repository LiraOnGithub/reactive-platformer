module Level where

import qualified Brick
import qualified Player
import Sprite
import Vector
import Event

data Level = Level 
	{ bricks :: [Brick.Brick]
	, player :: Player.Player
	}

initialLevel :: Level
initialLevel = Level
	{ bricks = [placeBrick 2 5, placeBrick 1 5, placeBrick 0 5]
	, player = Player.initialPlayer
	}

spritesToDraw :: Level -> [(Vec2 Int, SpriteInformation)]
spritesToDraw level = (getSpriteToDraw <$> level.bricks) <> [(getSpriteToDraw level.player)]

updateLevel :: PressedKeys -> (Level -> Level)
updateLevel pressedKeys level = level 
	{ player = updatedPlayer { Player.spriteInformation = setSpriteIndex updatedPlayer.spriteInformation }
	, bricks = (\b -> b { Brick.spriteInformation = setSpriteIndex b.spriteInformation }) <$> level.bricks
	}
	where
		updatedPlayer :: Player.Player
		updatedPlayer = Player.updatePlayer pressedKeys level.player

placeBrick :: Int -> Int -> Brick.Brick
placeBrick x y = Brick.Brick
	{ position = Vec2 (x * 20) (y * 20)
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

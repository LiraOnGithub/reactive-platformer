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
	{ bricks = [Brick.placeBrick 50 50, Brick.placeBrick 50 70]
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

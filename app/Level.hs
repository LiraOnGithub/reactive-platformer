module Level where

import qualified Brick
import qualified Player
import Sprite
import Vector
import Event
import Constants
import Collision.LevelGrid
import Collision.SurroundingElements
import Common.HasDefault

data Level = Level
	{ bricks :: LevelGrid Brick.Brick
	, player :: Player.Player
	}

initialLevel :: Level
initialLevel = Level
	{ player = getDefault
	, bricks = createLevelGrid 10 20 [placeBrick 0 180, placeBrick 20 180, placeBrick 40 180]
	}
	where
		placeBrick :: Double -> Double -> Brick.Brick
		placeBrick x y = getDefault { Brick.position = Vec2 x y }

spritesToDraw :: Level -> [(Vec2 Int, SpriteInformation)]
spritesToDraw level = (getSpriteToDraw <$> (getElements level.bricks)) <> [(getSpriteToDraw level.player)]

updateLevel :: PressedKeys -> (Level -> Level)
updateLevel pressedKeys level = updateSprites . handleCollisionDetection $ level
	{ player = Player.updatePlayer pressedKeys level.player
	}

updateSprites :: (Level -> Level)
updateSprites level = level
	{ player = level.player { Player.spriteInformation = setSpriteIndex level.player.spriteInformation }
	, bricks = (\b -> b { Brick.spriteInformation = setSpriteIndex b.spriteInformation }) <$> level.bricks
	}

handleCollisionDetection :: (Level -> Level)
handleCollisionDetection level = level
	{ player = handleCollisions level.player
	}
	where
		handleCollisions :: Player.Player -> Player.Player
		handleCollisions player
			 | abs player.velocity.y > abs player.velocity.x = horizontal . vertical $ player
			 | otherwise = vertical . horizontal $ player
		horizontal :: Player.Player -> Player.Player
		horizontal player
			 | mustMoveLeft player.velocity se = player
				{ Player.position = player.position { x = (roundToGrid player.position).x }
				, Player.velocity = player.velocity { x = 0 }
			 	}
			 | mustMoveRight player.velocity se = player
				{ Player.position = player.position { x = (roundToGrid player.position).x + gridSize }
				, Player.velocity = player.velocity { x = 0 }
			 	}
			| otherwise = player
			where
				se :: SurroundingElements Brick.Brick
				se = getSurroundingElements level.bricks player.position
		vertical :: Player.Player -> Player.Player
		vertical player
			| mustMoveUp player.velocity se = player
				{ Player.position = player.position { y = (roundToGrid player.position).y }
				, Player.velocity = player.velocity { y = 0 }
				, Player.groundState = Player.Grounded
				}
			| mustMoveDown player.velocity se = player
				{ Player.position = player.position { y = (roundToGrid player.position).y + gridSize }
				, Player.velocity = player.velocity { y = 0 }
				}
			| otherwise = player
			where
				se :: SurroundingElements Brick.Brick
				se = getSurroundingElements level.bricks player.position

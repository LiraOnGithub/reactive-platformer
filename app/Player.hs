module Player where

import Data.Bool (bool)
import Data.Ord (clamp)

import Brick (Brick)
import Event
import Sprite
import Vector
import Common.HasDefault
import Constants
import Collision.SurroundingElements
import Collision.LevelGrid

data GroundState
	= Airborne
	| Grounded
	deriving (Show, Eq)

data Player = Player
	{ position :: Vec2 Double
	, acceleration :: Double
	, friction :: Double
	, velocity :: Vec2 Double
	, speed :: Double
	, jumpPower :: Double
	, gravity :: Double
	, groundState :: GroundState
	, spriteInformation :: SpriteInformation
	} deriving Show

instance HasSprite Player where
	getSpritePosition player = round <$> player.position
	setSprite spriteInformation player = player { spriteInformation = spriteInformation }

instance HasDefault Player where
	getDefault = Player
		{ position = Vec2 20 20
		, acceleration = 1
		, friction = 2
		, velocity = Vec2 0 0
		, speed = 4
		, jumpPower = 12
		, gravity = 1
		, groundState = Airborne
		, spriteInformation = SpriteInformation
			{ sprite = PlayerSprite
			, previousAction = SpriteActionIdle
			, action = SpriteActionIdle
			, index = 0
			, width = 20
			, height = 20
			, repeats = True
			, counter = 0
			}
		}

groundPosition :: Double
groundPosition = 250

updatePlayer :: PressedKeys -> (Player -> Player)
updatePlayer k = updateSprite . handleGrounded . applyFriction . setPosition . applyAcceleration . applyGravity . handleJump
	where 
		setPosition :: Player -> Player
		setPosition player = player { position = Vec2
			{ x = player.position.x + player.velocity.x * k.deltaLastTick
			, y = min groundPosition (player.position.y + player.velocity.y * k.deltaLastTick)
			} }
			where
				changeLeft :: Double
				changeLeft = getSpeedInDirection k.left * k.deltaLastTick
				changeRight :: Double
				changeRight = getSpeedInDirection k.right * k.deltaLastTick
				getSpeedInDirection :: Bool -> Double
				getSpeedInDirection = bool 0 player.speed
		handleJump :: Player -> Player
		handleJump player
			| player.groundState == Grounded && k.up = player
				{ velocity = player.velocity { y = -player.jumpPower }
				, groundState = Airborne
				}
			| otherwise = player
		applyGravity :: Player -> Player
		applyGravity player = player
			{ velocity = player.velocity { y = player.velocity.y + player.gravity * k.deltaLastTick }
			}
		applyAcceleration :: Player -> Player
		applyAcceleration player = player
			{ velocity = player.velocity { x = clamp (-player.speed, player.speed) $ player.velocity.x - accelerationLeft + accelerationRight }
			}
			where
				accelerationLeft :: Double
				accelerationLeft = getAccelerationInDirection k.left * k.deltaLastTick
				accelerationRight :: Double
				accelerationRight = getAccelerationInDirection k.right * k.deltaLastTick
				getAccelerationInDirection :: Bool -> Double
				getAccelerationInDirection = bool 0 player.acceleration
		applyFriction :: Player -> Player
		applyFriction player = player
			{ velocity = player.velocity { x = player.velocity.x / player.friction }
			}
		handleGrounded :: Player -> Player
		handleGrounded player
			| player.position.y < groundPosition = player { groundState = Airborne }
			| otherwise = player { groundState = Grounded, velocity = player.velocity { y = 0 } }

updateSprite :: Player -> Player
updateSprite player = player { spriteInformation = (setSpriteAction player) . setPreviousSpriteAction $ player.spriteInformation }

setSpriteAction :: Player -> SpriteInformation -> SpriteInformation
setSpriteAction player spriteInformation = spriteInformation { action = getSpriteAction player }

getSpriteAction :: Player -> SpriteAction
getSpriteAction player
	| player.groundState == Airborne && player.velocity.y < 0 = SpriteActionJump
	| player.groundState == Airborne && player.velocity.y > 0 = SpriteActionFall
	| player.velocity.x > 0.4 = SpriteActionWalkRight
	| player.velocity.x < -0.4 = SpriteActionWalkLeft
	| otherwise = SpriteActionIdle

handleCollisions :: LevelGrid Brick -> Player -> Player
handleCollisions bricks player
	 | abs player.velocity.y > abs player.velocity.x
		= (handleCollisionHorizontal bricks) . (handleCollisionVertical bricks) $ player
	 | otherwise = (handleCollisionVertical bricks)  . (handleCollisionHorizontal bricks) $ player

se :: LevelGrid Brick -> Player -> SurroundingElements Brick.Brick
se bricks player = getSurroundingElements bricks player.position

handleCollisionHorizontal :: LevelGrid Brick -> Player -> Player
handleCollisionHorizontal bricks player
	 | mustMoveLeft player.velocity (se bricks player) = player
		{ position = player.position { x = (roundToGrid player.position).x }
		, velocity = player.velocity { x = 0 }
		}
	 | mustMoveRight player.velocity (se bricks player) = player
		{ position = player.position { x = (roundToGrid player.position).x + gridSize }
		, velocity = player.velocity { x = 0 }
		}
	| otherwise = player
handleCollisionVertical :: LevelGrid Brick -> Player -> Player
handleCollisionVertical bricks player
	| mustMoveUp player.velocity (se bricks player) = player
		{ position = player.position { y = (roundToGrid player.position).y }
		, velocity = player.velocity { y = 0 }
		, groundState = Grounded
		}
	| mustMoveDown player.velocity (se bricks player) = player
		{ position = player.position { y = (roundToGrid player.position).y + gridSize }
		, velocity = player.velocity { y = 0 }
		}
	| otherwise = player

module Player where

import Data.Bool (bool)
import Data.Ord (clamp)

import Event
import Sprite

data GroundState
	= Airborne
	| Grounded
	deriving (Show, Eq)

data Player = Player
	{ x :: Double
	, y :: Double
	, acceleration :: Double
	, friction :: Double
	, horizontalVelocity :: Double
	, speed :: Double

	, verticalVelocity :: Double
	, jumpPower :: Double
	, gravity :: Double
	, groundState :: GroundState

	, spriteInformation :: SpriteInformation
	} deriving Show

initialPlayer :: Player
initialPlayer = Player
	{ x = 20
	, y = 20
	, acceleration = 1
	, friction = 2
	, horizontalVelocity = 0
	, speed = 4

	, verticalVelocity = 0
	, jumpPower = 12
	, gravity = 1
	, groundState = Airborne

	, spriteInformation = SpriteInformation
		{ sprite = PlayerSprite
		, previousSpriteAction = SpriteActionIdle
		, spriteAction = SpriteActionIdle
		, spriteIndex = 0
		, width = 20
		, height = 20
		}
	}

groundPosition :: Double
groundPosition = 150

inAir :: Player -> Bool
inAir player = player.y < groundPosition

updatePlayer :: PressedKeys -> (Player -> Player)
updatePlayer k = updateSprite . handleGrounded . applyFriction . setPosition . applyAcceleration . applyGravity . handleJump
	where 
		setPosition :: Player -> Player
		setPosition player = player
			{ x = player.x + player.horizontalVelocity * k.deltaLastTick
			, y = min groundPosition (player.y + player.verticalVelocity * k.deltaLastTick)
			}
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
				{ verticalVelocity = -player.jumpPower
				, groundState = Airborne
				}
			| otherwise = player
		applyGravity :: Player -> Player
		applyGravity player = player 
			{ verticalVelocity = player.verticalVelocity + player.gravity * k.deltaLastTick
			}
		applyAcceleration :: Player -> Player
		applyAcceleration player = player
			{ horizontalVelocity = clamp (-player.speed, player.speed) $ player.horizontalVelocity - accelerationLeft + accelerationRight
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
			{ horizontalVelocity = player.horizontalVelocity / player.friction
			}
		handleGrounded :: Player -> Player
		handleGrounded player
			| inAir player = player { groundState = Airborne }
			| otherwise = player { groundState = Grounded, verticalVelocity = 0 }

updateSprite :: Player -> Player
updateSprite player = player { spriteInformation = setSpriteIndex . (setSpriteAction player) . setPreviousSprite $ player.spriteInformation }

setSpriteAction :: Player -> SpriteInformation -> SpriteInformation
setSpriteAction player spriteInformation = spriteInformation { spriteAction = getSpriteAction player }

getSpriteAction :: Player -> SpriteAction
getSpriteAction player
	| player.groundState == Airborne && player.verticalVelocity < 0 = SpriteActionJump
	| player.groundState == Airborne && player.verticalVelocity > 0 = SpriteActionFall
	| player.horizontalVelocity > 0.4 = SpriteActionWalkRight
	| player.horizontalVelocity < -0.4 = SpriteActionWalkLeft
	| otherwise = SpriteActionIdle

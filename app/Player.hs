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
	, width :: Double
	, height :: Double
	, acceleration :: Double
	, friction :: Double
	, horizontalVelocity :: Double
	, speed :: Double

	, verticalVelocity :: Double
	, jumpPower :: Double
	, gravity :: Double
	, groundState :: GroundState

	, sprite :: Sprite
	, previousSpriteAction :: SpriteAction
	, spriteAction :: SpriteAction
	, spriteIndex :: Int
	} deriving Show

initialPlayer :: Player
initialPlayer = Player
	{ x = 20
	, y = 20
	, width = 20
	, height = 20
	, acceleration = 1
	, friction = 2
	, horizontalVelocity = 0
	, speed = 4

	, verticalVelocity = 0
	, jumpPower = 12
	, gravity = 1
	, groundState = Airborne

	, sprite = PlayerSprite
	, previousSpriteAction = SpriteActionIdle
	, spriteAction = SpriteActionIdle
	, spriteIndex = 0
	}

groundPosition :: Double
groundPosition = 150

inAir :: Player -> Bool
inAir player = player.y < groundPosition

updatePlayer :: PressedKeys -> (Player -> Player)
updatePlayer k = setSpriteIndex . setSprite . setPreviousSprite . handleGrounded . applyFriction . setPosition . applyAcceleration . applyGravity . handleJump
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

setPreviousSprite :: Player -> Player
setPreviousSprite player = player { previousSpriteAction = player.spriteAction }

setSprite :: Player -> Player
setSprite player
	| player.groundState == Airborne && player.verticalVelocity < 0 = player { spriteAction = SpriteActionJump }
	| player.groundState == Airborne && player.verticalVelocity > 0 = player { spriteAction = SpriteActionFall }
	| player.horizontalVelocity > 0.4 = player { spriteAction = SpriteActionWalkRight }
	| player.horizontalVelocity < -0.4 = player { spriteAction = SpriteActionWalkLeft }
	| otherwise = player { spriteAction = SpriteActionIdle }

setSpriteIndex :: Player -> Player
setSpriteIndex player
	| player.previousSpriteAction /= player.spriteAction = player { spriteIndex = 0 }
	| otherwise = player { spriteIndex = (player.spriteIndex + 1) `mod` frameCount player.sprite player.spriteAction }

module Player where

import Data.Bool (bool)
import Event

data GroundState
	= Airborne
	| Grounded
	deriving (Show, Eq)

data Player = Player
	{ x :: Double
	, y :: Double
	, width :: Double
	, height :: Double

	, speed :: Double

	, verticalVelocity :: Double
	, jumpPower :: Double
	, gravity :: Double
	, groundState :: GroundState
	} deriving Show

initialPlayer :: Player
initialPlayer = Player
	{ x = 20
	, y = 20
	, width = 20
	, height = 20

	, speed = 4

	, verticalVelocity = 0
	, jumpPower = 12
	, gravity = 1
	, groundState = Airborne
	}

groundPosition :: Double
groundPosition = 150

inAir :: Player -> Bool
inAir player = player.y < groundPosition

updatePlayer :: PressedKeys -> (Player -> Player)
updatePlayer k = handleGrounded . setPosition . applyGravity . handleJump
	where 
		setPosition :: Player -> Player
		setPosition player = player 
			{ x = player.x - changeLeft + changeRight
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
		handleGrounded :: Player -> Player
		handleGrounded player
			| inAir player = player { groundState = Airborne }
			| otherwise = player { groundState = Grounded, verticalVelocity = 0 }

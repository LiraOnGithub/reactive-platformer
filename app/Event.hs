module Event where

data Event
	= ArrowUpEvent Bool
	| ArrowRightEvent Bool
	| ArrowLeftEvent Bool
	| TickEvent Double
	| QuitEvent

data PressedKeys = PressedKeys
	{ up :: Bool
	, right :: Bool
	, left :: Bool
	, deltaLastTick :: Double
	, executeTick :: Bool
	}

initialPressedKeys :: PressedKeys
initialPressedKeys = PressedKeys
	{ up = False
	, right = False
	, left = False
	, deltaLastTick = 0
	, executeTick = False
	}

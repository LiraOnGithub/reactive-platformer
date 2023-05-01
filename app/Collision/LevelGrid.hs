module Collision.LevelGrid where

import Data.Matrix (Matrix, matrix, toList, setElem, (!))
import Data.Maybe (mapMaybe)

import Vector
import Constants
import Brick
import Collision.SurroundingElements

newtype LevelGrid a = LevelGrid (Matrix (Maybe a)) deriving Show

instance Functor LevelGrid where
	fmap f (LevelGrid matrix) = LevelGrid ((fmap . fmap) f matrix)

createLevelGrid :: Int -> Int -> [Brick] -> LevelGrid Brick
createLevelGrid width height xs = LevelGrid $ foldr setElem' emptyMatrix xs
	where
		setElem' :: Brick -> Matrix (Maybe Brick) -> Matrix (Maybe Brick)
		setElem' block = setElem (Just block) (coordinateInMatrix block.position)
		emptyMatrix :: Matrix (Maybe Brick)
		emptyMatrix = matrix width height (const Nothing)

getSurroundingElements :: LevelGrid a -> Vec2 Double -> SurroundingElements a
getSurroundingElements (LevelGrid elements) v
	| not horizontallySnappedToGrid && not verticallySnappedToGrid = SurroundingElements
		{ topLeft = elements ! coordinateInMatrix v
		, topRight = elements ! coordinateInMatrix (v + Vec2 gridSize 0)
		, bottomLeft = elements ! coordinateInMatrix (v + Vec2 0 gridSize)
		, bottomRight = elements ! coordinateInMatrix (v + Vec2 gridSize gridSize)
		}
	| not horizontallySnappedToGrid && verticallySnappedToGrid = SurroundingElementsHorizontal
		{ left = elements ! coordinateInMatrix v
		, right = elements ! coordinateInMatrix (v + Vec2 gridSize 0)
		}
	| horizontallySnappedToGrid && not verticallySnappedToGrid = SurroundingElementsVertical
		{ top = elements ! coordinateInMatrix v
		, bottom = elements ! coordinateInMatrix (v + Vec2 0 gridSize)
		}
	| otherwise = NoSurroundingElements
	where
		horizontallySnappedToGrid :: Bool
		horizontallySnappedToGrid = round v.x `mod` gridSize == 0
		verticallySnappedToGrid :: Bool
		verticallySnappedToGrid = round v.y `mod` gridSize == 0

-- matrix needs a 1-based coordinate in tuple-form
coordinateInMatrix :: Vec2 Double -> (Int, Int)
coordinateInMatrix v = ((coordinateOnGrid v).x + 1, (coordinateOnGrid v).y + 1)

coordinateOnGrid :: Vec2 Double -> Vec2 Int
coordinateOnGrid v = floor <$> (/ fromIntegral gridSize) <$> v

roundToGrid :: Vec2 Double -> Vec2 Double
roundToGrid v = (* gridSize) <$> fromIntegral <$> coordinateOnGrid v

getElements :: LevelGrid a -> [a]
getElements (LevelGrid matrix) = mapMaybe id (toList matrix)

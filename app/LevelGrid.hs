module LevelGrid where

import Data.Matrix (Matrix, matrix, toList, setElem, (!))
import Data.Maybe (mapMaybe)

import Vector
import Constants
import Brick

newtype LevelGrid a = LevelGrid (Matrix (Maybe a)) deriving Show

instance Functor LevelGrid where
	fmap f (LevelGrid matrix) = LevelGrid ((fmap . fmap) f matrix)

createLevelGrid :: Int -> Int -> [Brick] -> LevelGrid Brick
createLevelGrid width height xs = LevelGrid $ foldr setElem' emptyMatrix xs
	where
		setElem' :: Brick -> Matrix (Maybe Brick) -> Matrix (Maybe Brick)
		setElem' block = setElem (Just block) (coordinateOnGrid block.position)
		emptyMatrix :: Matrix (Maybe Brick)
		emptyMatrix = matrix width height (const Nothing)

data SurroundingElements a = SurroundingElements
	{ topLeft :: Maybe a
	, topRight :: Maybe a
	, bottomLeft :: Maybe a
	, bottomRight :: Maybe a
	}
	deriving Show

getSurroundingElements :: LevelGrid a -> Vec2 Double -> SurroundingElements a
getSurroundingElements (LevelGrid elements) v = SurroundingElements
	{ topLeft = elements ! coordinateOnGrid v
	, topRight = elements ! coordinateOnGrid (v + Vec2 gridSize 0)
	, bottomLeft = elements ! coordinateOnGrid (v + Vec2 0 gridSize)
	, bottomRight = elements ! coordinateOnGrid (v + Vec2 gridSize gridSize)
	}

coordinateOnGrid :: Vec2 Double -> (Int, Int)
coordinateOnGrid v = (roundedCoordinate.x + 1, roundedCoordinate.y + 1)
	where
		roundedCoordinate :: Vec2 Int
		roundedCoordinate = floor <$> (/ fromIntegral gridSize) <$> v

getElements :: LevelGrid a -> [a]
getElements (LevelGrid matrix) = mapMaybe id (toList matrix)

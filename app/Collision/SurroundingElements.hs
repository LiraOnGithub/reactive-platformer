module Collision.SurroundingElements where

import Data.Bool (bool)
import Data.Maybe (isJust)

import Vector
import Constants

{-
   Possible situations:
	* object overlaps 4 grid cells
	* object overlaps 2 grid cells, horizontally
	* object overlaps 2 grid cells, vertically
	* object fits in 1 grid cell
   -}
data SurroundingElements a
	= SurroundingElements
		{ topLeft :: Maybe a
		, topRight :: Maybe a
		, bottomLeft :: Maybe a
		, bottomRight :: Maybe a
		}
	| SurroundingElementsHorizontal
		{ left :: Maybe a
		, right :: Maybe a
		}
	| SurroundingElementsVertical
		{ top :: Maybe a
		, bottom :: Maybe a
		}
	| NoSurroundingElements
	deriving Show

mustMoveUp :: Vec2 Double -> SurroundingElements a -> Bool
mustMoveUp velocity SurroundingElements {bottomLeft, bottomRight} =
	velocity.y >= 0 && (isJust bottomLeft || isJust bottomRight)
mustMoveUp velocity SurroundingElementsVertical { bottom } = velocity.y >= 0 && isJust bottom
mustMoveUp _ _ = False

mustMoveDown :: Vec2 Double -> SurroundingElements a -> Bool
mustMoveDown velocity SurroundingElements {topLeft, topRight} =
	velocity.y <= 0 && (isJust topLeft || isJust topRight)
mustMoveDown velocity SurroundingElementsVertical { top } = velocity.y <= 0 && isJust top
mustMoveDown _ _ = False

mustMoveLeft :: Vec2 Double -> SurroundingElements a -> Bool
mustMoveLeft velocity SurroundingElements {topRight, bottomRight} =
	velocity.x >= 0 && (isJust topRight || isJust bottomRight)
mustMoveLeft velocity SurroundingElementsHorizontal { right } = velocity.x >= 0 && isJust right
mustMoveLeft _ _ = False

mustMoveRight :: Vec2 Double -> SurroundingElements a -> Bool
mustMoveRight velocity SurroundingElements {topLeft, bottomLeft} =
	velocity.x <= 0 && (isJust topLeft || isJust bottomLeft)
mustMoveRight velocity SurroundingElementsHorizontal { left } = velocity.x <= 0 && isJust left
mustMoveRight _ _ = False

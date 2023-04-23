module Vector where

data Vec2 a = Vec2 { x :: a, y :: a } deriving (Eq, Show)

instance Functor Vec2 where
	fmap f (Vec2 { x, y }) = Vec2 (f x) (f y)

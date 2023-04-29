module Vector where

import Control.Applicative (liftA2)

--Linear.V2 does not work with OverloadedRecordDot
data Vec2 a = Vec2 { x :: a, y :: a } deriving (Eq, Show)

instance Functor Vec2 where
	fmap f (Vec2 { x, y }) = Vec2 (f x) (f y)
	{-# INLINE fmap #-}

instance Applicative Vec2 where
	pure a = Vec2 a a
	{-# INLINE pure #-}
	Vec2 a b <*> Vec2 d e = Vec2 (a d) (b e)
	{-# INLINE (<*>) #-}

instance Num a => Num (Vec2 a) where
	(+) = liftA2 (+)
	{-# INLINE (+) #-}
	(-) = liftA2 (-)
	{-# INLINE (-) #-}
	(*) = liftA2 (*)
	{-# INLINE (*) #-}
	negate = fmap negate
	{-# INLINE negate #-}
	abs = fmap abs
	{-# INLINE abs #-}
	signum = fmap signum
	{-# INLINE signum #-}
	fromInteger = pure . fromInteger
	{-# INLINE fromInteger #-}

module Utils where

import Prelude

sign :: Double -> Double
sign x
	| x < 0     = -1
	| x > 0     =  1
	| otherwise =  0

-- | Monadic version of 'when'.
whenM :: Fay Bool -> Fay () -> Fay ()
whenM cond act = cond >>= flip when act

-- | Monadic version of 'unless'.
unlessM :: Fay Bool -> Fay () -> Fay ()
unlessM cond act = cond >>= flip unless act

-- | Fay version of 'liftM'.
liftM :: (a -> b) -> Fay a -> Fay b
liftM f a = a >>= return . f

-- | Reverse ($).
x |> f = f x

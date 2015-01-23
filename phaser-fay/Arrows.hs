module Arrows where

first  f (x, y) = (f x,   y)
second f (x, y) = (  x, f y)

-- Composition
f >>> g = g . f

-- Combinators
--f *** g = \(x, y) -> (f x, f y)
f *** g = first f >>> second g

f &&& g = \x -> (f x, g x)

-- Splitting
split x = (x, x)

unsplit = uncurry

liftA2 op f g = f &&& g >>> unsplit op

liftA2_ = liftA2 . const . const $ return ()

-- Application
val ~> arr = arr val

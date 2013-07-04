--- ---------------------------------------------------------------------------
--- A combination of Error and state monad like `ErrorT State` in Haskell
---
--- @author Björn Peemöller
--- @version July 2013
--- ----------------------------------------------------------------------------

module ErrorState where

--- Error state monad.
type ES e s a = s -> Either e (a, s)

--- Evaluate an `ES` monad
evalES :: ES e s a -> s -> Either e a
evalES m s = case m s of
  Left  e      -> Left e
  Right (x, _) -> Right x

--- Lift a value into the `ES` monad
returnES :: a -> ES e s a
returnES x s = Right (x, s)

--- Bind of the `ES` monad
(>+=) :: ES e s a -> (a -> ES e s b) -> ES e s b
(m >+= f) s = case m s of
  Left  e       -> Left e
  Right (x, s') -> f x s'

--- Sequence operator of the `ES` monad
(>+) :: ES e s a -> ES e s b -> ES e s b
m >+ n = m >+= \_ -> n

--- Failing computation in the `ES` monad
failES :: e -> ES e s a
failES e _ = Left e

--- Retrieve the current state
gets :: ES e s s
gets s = Right (s, s)

--- Replace the current state
puts :: s -> ES e s ()
puts s _ = Right ((), s)

--- Modify the current state
modify :: (s -> s) -> ES e s ()
modify f s = Right ((), f s)

--- Lift the given unary function into the monad.
liftES :: (a -> b) -> ES e s a -> ES e s b
liftES f m = m >+= (returnES . f)

--- Lift the given binary function into the monad.
liftES2 :: (a -> b -> c) -> ES e s a -> ES e s b -> ES e s c
liftES2 f m n = m >+= \x -> n >+= \y -> returnES (f x y)

--- Lift the given ternary function into the monad.
liftES3 :: (a -> b -> c -> d) -> ES e s a -> ES e s b -> ES e s c -> ES e s d
liftES3 f m n o = m >+= \x -> n >+= \y -> o >+= \z -> returnES (f x y z)

--- Map a monadic function on all elements of a list by sequencing
--- the effects.
mapES :: (a -> ES e s b) -> [a] -> ES e s [b]
mapES _ []       = returnES []
mapES f (x : xs) = f x        >+= \y  ->
                   mapES f xs >+= \ys ->
                   returnES (y:ys)

--- Same as `concatMap`, but for a monadic function.
concatMapES :: (a -> ES e s [b]) -> [a] -> ES e s [b]
concatMapES f xs = concat `liftES` mapES f xs

--- Same as `mapES` but with an additional accumulator threaded.
mapAccumES :: (a -> b -> ES e s (a, c)) -> a -> [b] -> ES e s (a, [c])
mapAccumES _ s []       = returnES (s, [])
mapAccumES f s (x : xs) = f s x >+= \(s', y) ->
                          mapAccumES f s' xs >+= \(s'', ys) ->
                          returnES (s'', y:ys)
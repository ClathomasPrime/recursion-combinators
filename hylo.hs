module Hylo
  (
  ) where

import Prelude hiding(foldr)
import Ana
import Cata

hylo :: c -> (b -> c -> c) -> (a -> Maybe (b,a)) -> a -> c
hylo c down up a = foldr c down $ unfold up a     -- | build up a list, then collapse it down

hylo' :: c -> (b -> c -> c) -> (a -> Maybe (b,a)) -> a -> c
hylo' cap down up seed = case up seed of                      -- | Build a level from `seed`.
                              Nothing -> cap                  -- | If done, return `cap`.
                              Just (b,a) ->                   -- | If not yet done, send out a `b`,
                                down b $ hylo' cap down up a  -- | then collapse with the `b` and the result of the new seed `a`.


fact :: (Num a, Ord a) => a -> a
fact = hylo 1 (*) posRec

posRec a | a <= 0    = Nothing
         | otherwise = Just (a, a-1)


--(>>=) :: Monad f       => f a ->   (  a -> f b) -> f b
--(*>=) :: Applicative f => f a -> f (  a ->   b) -> f b
--($>=) :: Functor f     => f a ->   (  a ->   b) -> f b


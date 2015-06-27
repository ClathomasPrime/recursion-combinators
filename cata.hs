module Cata
  ( foldr
  ) where

import Prelude hiding(foldr, length, filter, map)

foldr :: b -> (a -> b -> b) -> [a] -> b
foldr b _ [] = b
foldr b f (a:as) = f a (foldr b f as)


length :: Num b => [a] -> b
length = foldr 0 (\_ b -> b+1)


filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr [] acc
  where acc a accepted 
            | p a = a : accepted
            | otherwise = accepted


map :: (a -> b) -> [a] -> [b]
map f = foldr [] (\a bs -> f a : bs)

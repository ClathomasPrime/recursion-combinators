module Ana
  ( unfold
  ) where

import Prelude hiding(zip, iterate, map)

unfold :: (a -> Maybe (b,a)) -> a -> [b]
unfold phi seed = case phi seed of 
                       Nothing -> []
                       Just (b,a) -> b : unfold phi a



zip :: [a] -> [b] -> [(a,b)]
zip = curry $ unfold zip'
  where zip' ([],_) = Nothing
        zip' (_,[]) = Nothing
        zip' (a:as,b:bs) = Just ((a,b), (as,bs))


iterate1 :: (a -> a) -> a -> [a]
iterate1 f = unfold (Just . dup . f)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (Just . \a -> (a, f a))

dup :: a -> (a,a)
dup a = (a,a)


map :: (a -> b) -> [a] -> [b]
map f = unfold phi
  where phi [] = Nothing
        phi (a:as) = Just (f a, as)



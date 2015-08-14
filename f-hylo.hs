
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo collapse build seed 
  = collapse . fmap (hylo collapse build) . build $ seed


treeSum :: Int -> Int
treeSum = hylo collapse build
  where build a = (a,a-1)
        collapse (1,_) = 1
        collapse (a,b) = a * collapse b

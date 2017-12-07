module HAD.Y2014.M02.D24.Exercise where

import Data.List

-- | Filter a list, keeping an element only if it is equal to the next one.
--
-- Examples:
-- >>> filterByPair []
-- []
-- >>> filterByPair [1 .. 10]
-- []
-- >>> filterByPair [1, 2, 2, 2, 3, 3, 4]
-- [2,2,3]
filterByPair :: Eq a => [a] -> [a]
filterByPair xs = group xs >>= init

filterByPair' :: Eq a => [a] -> [a]
filterByPair' = fst .
  foldr (\x (acc, lastX) ->
           case lastX of
             Just lastX' ->
                (if x == lastX' then x : acc else acc, Just x)
             _ ->
               (acc, Just x)
        ) ([], Nothing)

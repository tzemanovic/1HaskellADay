module HAD.Y2014.M02.D26.Exercise where

-- | Sum the value inside the maybe if there aren't any Nothing,
-- otherwise return Nothing
--
-- Examples
--
-- >>> sumIfAll [Just 1, Just 2]
-- Just 3
--
-- >>> sumIfAll [Just 1, Nothing]
-- Nothing
--
sumIfAll :: Num a => [Maybe a] -> Maybe a
sumIfAll = (sum <$>) . sequence

sumIfAll' :: Num a => [Maybe a] -> Maybe a
sumIfAll' = (sum <$>) . foldr (\x acc ->
                    case (x, acc) of
                      (Just x, Just acc) -> Just (x : acc)
                      _ -> Nothing
                    ) (Just [])

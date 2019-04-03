module Mirza.EntityDataAPI.Utils where



-- | Extract one element from a list.
-- Function implementation copied from:
-- https://hackage.haskell.org/package/raft-0.3.7.0/docs/Data-List-Util.html
extract :: (a -> Bool) -> [a] -> ([a], Maybe a)
extract _ [] = ([], Nothing)
extract p x =
  let
    extract' a @ (_, _, Just _) = a
    extract' a @ (_, [], Nothing) = a
    extract' (y', ze : zs, Nothing)
      | p ze = (y', zs, Just ze)
      | otherwise = extract' (ze : y', zs, Nothing)
    (y, z, w) = extract' ([], x, Nothing)
  in
    (rollback y z, w)


-- | Reverse a first list and add it to a second one.
rollback ::
      [a] -- ^ The list to be reversed and prepended.
   -> [a] -- ^ The list to be appended.
   -> [a] -- ^ The resulting list
rollback = flip (foldl (flip (:)))

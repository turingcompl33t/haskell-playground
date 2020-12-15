-- quicksort.hs
-- Quicksort sorting algorithm.

-- Quicksort in Haskell must be the most beautiful
-- implementation out there, it corresponds perfectly
-- with the mental model of the algorithm

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = let
  lower = quicksort [x | x <- xs, x <= p]
  upper = quicksort [x | x <- xs, x > p]
  in lower ++ [p] ++ upper
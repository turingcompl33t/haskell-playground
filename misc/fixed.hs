{-
    fixed.hs
    fun with fixed points
-}

-- poor re-implementation of haskell's 'fix' function
fixedpoint :: Eq a => (a -> a) -> a -> a 
fixedpoint f x 
    | x == fx   = fx 
    | otherwise = fixedpoint f fx
    where fx = f x

-- compute the square root function via fixed points
mysqrt :: Double -> Double 
mysqrt n = fixedpoint (\x -> (x + n/x) / 2) 1

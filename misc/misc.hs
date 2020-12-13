-- sum a list
sumlist :: [Int] -> Int
sumlist (x:xs) = x + sumlist xs
sumlist []     = 0

-- list product
prod :: [Int] -> Int 
prod [] = 1
prod (x:xs) = x * prod xs 

-- reverse a list
revlist :: [Int] -> [Int]
revlist (x:xs) = revlist xs ++ [x]
revlist []     = []

-- factorial 
fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1) 

-- another factorial, in terms of product
fac' :: (Integral a) => a -> a 
fac' 0 = 1
fac' n = product [1..n]

-- quicksort
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where 
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x ]
   
-- implement the zip function
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (a:as) (b:bs) = (a,b) : myzip as bs

-- replicate value a, n times
rep :: Int -> a -> [a]
rep 0 _ = []
rep n x = x : replicate (n - 1) x

-- implement the (!!) operator
idx :: Int -> [a] -> Maybe a 
idx _ []     = Nothing
idx 0 (x:xs) = Just x 
idx n (x:xs) = idx (n - 1) xs

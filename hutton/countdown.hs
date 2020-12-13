{- 
  countdown.hs 
  solve the countdown problem 
-}

import Data.List

{------------------------------------------------------------------------------
  Type Definitions 
-}

-- type definition for expression operators
data Op = Add | Sub | Mul | Div 
instance Show Op where 
  show Add = show "+"
  show Sub = show "-"
  show Mul = show "*"
  show Div = show "/"

-- type definition for arithmetic expression
data Expr = Val Int | App Op Expr Expr 

-- type alias 
-- a pair of a valid expression and its value
type Result = (Expr, Int)

{------------------------------------------------------------------------------
  Problem Domain Functions
-}

-- apply a binary operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y 
apply Mul x y = x * y 
apply Div x y = x `div` y

-- determine if the result of binary operator application is valid
valid :: Op -> Int -> Int -> Bool 
valid Add x y = x <= y                      -- exploit commutativity of addition 
valid Sub x y = x > y 
valid Mul x y = x <= y && x /= 1 && y /= 1  -- exploit commutativity, identity
valid Div x y = x `mod` y == 0 && y /= 1    -- exploit identity 

-- evaluate an expression 
-- returns either empty list or singleton list
eval :: Expr -> [Int] 
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l 
                                , y <- eval r 
                                , valid o x y ]

-- return list of all values in expression                               
values :: Expr -> [Int]
values (Val n)     = [n]
values (App o l r) = values l ++ values r

-- return a list of all possible expressions whose values
-- are precisely the given list of numbers
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns 
               , l <- exprs ls 
               , r <- exprs rs 
               , e <- combine l r ]

-- same as exprs, but now packages them up with their value 
-- an example of 'program fusion' to improve performance 
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns
                   , lx <- results ls
                   , rx <- results rs
                   , res <- combine' lx rx ]
             
-- combine two expressions in all possible ways  
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

-- combine two results into all possible expressions 
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div], valid o x y]

-- return a list of all expressions that solve an instance 
-- of the countdown problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns 
                    , e <- exprs ns'
                    , eval e == [n] ]

-- the optimized solutions generator,
-- using results rather than expressions
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [ e | ns' <- choices ns 
                      , (e, m) <- results ns' 
                      , m == n ]

{------------------------------------------------------------------------------
  Helper Functions
-}

-- return list of all possible ways of splitting a list 
-- into two non-empty sublists           
split :: [a] -> [([a], [a])]
split xs = [(take i xs, drop i xs) | i <- [1..len xs - 1]]

-- generate all choices
choices :: [a] -> [[a]]
choices xs = flatten $ map permutations (allcombos xs)

-- generate all combinations of list elements
allcombos :: [a] -> [[a]]
allcombos xs = flatten [ combos i xs | i <- [0..len xs]]

-- generate all combinations of size n
combos :: Int -> [a] -> [[a]]
combos 0 _      = [[]]
combos _ []     = []
combos n (x:xs) = map (x:) (combos (n-1) xs) ++ combos n xs

-- generate all rotations of list 
rotations :: [a] -> [[a]]   
rotations [] = [[]]
rotations as = [xs | xs <- [rotate i as | i <- [0..l] ]]
               where l = len as - 1

-- rotate a list by n positions 
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = drop n' xs ++ take n' xs
            where n' = n `mod` len xs  

-- flatten a nested list       
flatten :: [[a]] -> [a]
flatten []       = []
flatten [[a]]    = [a]
flatten (xs:xss) = xs ++ flatten xss

-- determine length of a list recursively 
len :: [a] -> Int 
len []     = 0
len (a:as) = 1 + len as

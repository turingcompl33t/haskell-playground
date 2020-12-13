{-
 - tautology.hs
 - Tautology checker from Hutton's "Programming in Haskell
 -
 - Kyle Dotterrer
 - April, 2019
 -}

-- associative list
type Assoc k v = [(k, v)]

-- return value associated with first occurence of key in associative list
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- substitution lookup table utilizing the associative list
-- that is, we associate variables (Char) with boolean values
type Subst = Assoc Char Bool

-- proposition ADT
data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop
    deriving Show

-- A AND ~B
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'B'))

-- (A AND B) => A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A => (A AND B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A AND (A => B)) => B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- evaluate a proposition under a given substitution
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- return list of all variables in a proposition
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- enumerate all logical permutations of given length
-- basically just counting up in binary, 0 = False, 1 = True
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n - 1)

-- generate all possible substitutions for a proposition
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

-- remove duplicates from a list
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

-- determine if a proposition is a tautology
istaut :: Prop -> Bool
istaut p = and [eval s p | s <- substs p]

-- bool.hs
-- Fun with boolean values and operators. 

mynot :: Bool -> Bool

-- exhaustive 
-- mynot True  = False
-- mynot False = True

-- with conditionals 
-- mynot x = if x == True then False else True

-- with conditionals and lambda 
mynot = \x -> if x == True then False else True

myand :: Bool -> Bool -> Bool
myand True  = \x -> x
myand False = \x -> False

myor :: Bool -> Bool -> Bool 
myor True  = \x -> True
myor False = \x -> x

myxor :: Bool -> Bool -> Bool

-- the brute-force approach
-- myxor True True   = False
-- myxor True False  = True
-- myxor False True  = True 
-- myxor False False = False

-- slightly more refined 
myxor :: Bool -> Bool -> Bool 
myxor True  = \x -> mynot x
myxor False = \x -> x

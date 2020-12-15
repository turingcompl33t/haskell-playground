-- bool.hs
-- Fun with boolean values and operators. 

mynot :: Bool -> Bool
mynot = not

myand :: Bool -> Bool -> Bool
myand True  = id
myand False = const False

myor :: Bool -> Bool -> Bool 
myor True  = const True
myor False = id

myxor :: Bool -> Bool -> Bool 
myxor True  = mynot
myxor False = id

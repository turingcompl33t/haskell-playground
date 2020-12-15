{-
  list.hs
  A simple list implementation.
-}

data List a = Nil | Cons a (List a)
  deriving Show

-- head
myHead :: List a -> a
myHead l = case l of 
  Cons x _ -> x

-- last
myLast :: List a -> a
myLast l = case l of
  Cons x Nil -> x
  Cons _ xs  -> myLast xs 

-- init
myInit :: List a -> List a
myInit l = case l of
  Cons _ Nil -> Nil
  Cons x xs  -> Cons x (myInit xs)

-- tail
myTail :: List a -> List a
myTail Nil = Nil
myTail l   = case l of
    Cons _ xs -> xs

-- length
myLength :: List a -> Int
myLength Nil = 0
myLength l = case l of
  Cons _ xs -> 1 + myLength xs

-- zip
myZip :: List a -> List b -> List (a, b)
myZip Nil _ = Nil
myZip _ Nil = Nil
myZip k l = Cons (x, y) (myZip xs ys) where
  Cons x xs = k
  Cons y ys = l

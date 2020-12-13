{-
    statem.hs
    exploration of the state monad
-}

-- an arbitrary, stateful value
-- modeled here as an integer
type State = Int

-- a state transformer is a function
-- input:  state 
-- output: (arbitrary value of type a, updated state)
-- type ST a = State -> (a, State)

-- need to redo the type definition with newtype 
-- in order to make state transformer an instance of monad
newtype ST a = S (State -> (a, State))

-- we had to add a constructor for the state transformer function
-- this function just unwraps the state transformer from its constructor, 
-- and then applies it to the state we pass it to actually perform the transformation
-- this is why it is called app:
-- this function applies a state transformer (a function) to a state
app :: ST a -> State -> (a, State)
app (S st) x = st x 

-- if we want ST to be an instance of monad, 
-- it must first be an instance of functor
instance Functor ST where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

-- what does fmap do here? 
-- fmap allows us to apply a function (g) to the result value of ST function
-- so this application of fmap to a function g and a ST st just creates a new
-- state transformer in which g is mapped over the associated value before return
-- NOTE: fmap gives us back a new state transformer
--  it has to! it is a functor - it can't change the container, just whats inside it

-- make ST an instance of applicative functor
instance Applicative ST where 
    -- pure :: a -> f a
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))

    -- <*> :: f (a -> b) -> f a -> f b
    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f, s')  = app stf s 
            (x, s'') = app stx s'
        in (f x, s''))

-- first, what does pure do here?
-- just takes an input value x of arbitrary type and 
-- constructs a new state transformer function that returns x 
-- as the associated value, state is unmodified 

-- now, what does <*> do?
-- stf is a state transformer parametrized by a function
--  that is, it is a function that takes a state and returns (function, newstate)
-- stx is a state transformer parametrized by some concrete type, like usual
-- thus, stf <*> stx returns a new ST that behaves as follows:
--  apply ST stf to input state to get (f, s') where f is a function
--  apply ST stx to intermediate state s' to get (x, s'') where x is a value of type a
--  return a pair (y, s'') where s'' is the final state, 
--  y = f x is the result of applying f to x, of type b

-- make ST an instance of monad
instance Monad ST where 
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- what does >>= do? 
-- st is a state transformer that returns a value of type a
-- f is a function that takes a value of type a, returns a monadic b
--  that is, it returns a state transformer that returns a value of type b
-- thus, st >>= f returns a new ST that behaves as follows:
--  apply st to initial state to get (x, s')
--  apply f :: a -> ST b to the value intermediate value x to get a new ST
--  apply this new intermediate ST to intermediate state s' to get final result

data Tree a = Leaf a | Node (Tree a) (Tree a) 
              deriving Show

t :: Tree Char
t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- the next fresh integer state transformer 
-- returns the current state as the value, and the updated (incremented) state 
fresh :: ST Int
fresh = S (\n -> (n, n+1))

-- relabel the nodes of tree in applicative style 
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- relabel the nodes of tree in monadic style
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')


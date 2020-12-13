{-
    parsem.hs
    monadic parsing 
-}

import Data.Char
import Control.Applicative

-- want to be able to parse a string into explicit hierarchy
-- a tree is a good model for this hierarchy
data Tree a = Leaf a | Node (Tree a) (Tree a) 
              deriving Show

-- a parser for type a:
-- a function from a string to a list of tuples:
--  each tuple contains the result value of type a, and the unparsed remainder string
-- an empty list result denotes a failed parse
newtype Parser a = P (String -> [(a, String)])

-- apply a parser (function) to input string
parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

-- item: a simple parser definition
-- fails if input string empty,
-- parses first char of input string on success
item :: Parser Char
item = P (\input -> case input of
                        []     -> []
                        (x:xs) -> [(x, xs)])

-- make the parser type into instance of functor:
-- map function g over a parser
-- construct a new parser that applies g to result value before return
-- propogate failure if failure occurs in parse
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\input -> case parse p input of
                               []         -> []
                               [(v, out)] -> [(g v, out)])


-- make parser type into instance of applicative functor:
-- pure constructs new parser for input value that always just
--  gives back the argument value as resulting output
-- <*> takes applies a parser that returns a function to a parser that returns a value
--  to construct a parser that returns the result of applying the function to the value
-- <*> constructs a new parser that behaves as follows:
--  pg is a parser that returns a function of type (a -> b) as its value
--  px is a parser that returns a value of type a as its value
--  apply pg to input to get either failure or a function g (a -> b) and remainder
--  map g over px to get a new parser, and apply this to intermediate output
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\input -> [(x, input)])

    -- <*> :: (Parser (a -> b)) -> Parser a -> Parser b
    pg <*> px = P (\input -> case parse pg input of
                                 []         -> []
                                 [(g, out)] -> parse (fmap g px) out)


-- three parser, in applicative style
threeA :: Parser (Char, Char)
threeA = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

-- make the parer type into a monad
instance Monad Parser where
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\input -> case parse p input of
                           []         -> []
                           [(v, out)] -> parse (f v) out)

-- three parser, in monadic style
-- the do notation hides the sequencing:
-- the output of each parse in the sequence becomes the input to following parse, etc. 
threeM :: Parser (Char, Char)
threeM = do
    x <- item
    item
    z <- item
    return (x, z)

-- make parser an instance of alternative
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\input -> [])

    -- <|> :: Parser a -> Parser a -> Parser a
    p <|> q = P (\input -> case parse p input of 
                           []         -> parse q input
                           [(v, out)] -> [(v, out)])

-- define a parser for single characters that satisfy a predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- now we can use sat to define a number of other useful parsers

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- variable names
-- lowercase letter, followed by zero or more alphanumerics
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- natural numbers
-- 1 or more digits 
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- spacing
-- zero or more whitespace characters
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- integer values
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat


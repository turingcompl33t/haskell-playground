-- reverse polish notation calculator 
-- from LYAH

import Data.List

rpn :: String -> Float
rpn = head . foldl foldingfunc [] . words
  where foldingfunc (x:y:ys) "*" = (x*y):ys 
        foldingfunc (x:y:ys) "+" = (x+y):ys 
        foldingfunc (x:y:ys) "-" = (y-x):ys 
        foldingfunc (x:y:ys) "/" = (y/x):ys
        foldingfunc (x:y:ys) "^" = (y**x):ys
        foldingfunc (x:xs) "ln"  = log x:xs
        foldingfunc xs "sum"     = [sum xs]
        foldingfunc xs numstr    = read numstr:xs 

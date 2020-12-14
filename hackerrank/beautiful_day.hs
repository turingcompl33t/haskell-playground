-- beautiful_day.hs

main = do 
  [i, j, k] <- getIntList
  print $ solve i j k

solve :: Int -> Int -> Int -> Int
solve i j k = length $ [x | x <- [i..j], (x - reverseInt x) `mod` k == 0]

getIntList :: IO [Int]
getIntList = do map read . words <$> getLine

reverseInt :: Int -> Int
reverseInt = read . reverse . show 
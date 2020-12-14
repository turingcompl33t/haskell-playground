-- find_digits.hs

main = interact $ unlines . map (show . countDivisors) . tail . lines

countDivisors :: String -> Int
countDivisors s = length $ filter (\x -> x /= 0 && n `mod` x == 0) (digits s)
  where n = read s

digits :: String -> [Int]
digits = map (read . pure :: Char -> Int)
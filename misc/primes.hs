{-
	primes.hs
	fun with infinite data structures
-}

primes = sieve [2..]
tprimes = filter twin $ zip primes (tail primes)

-- sieve of eratosthenes
sieve :: [Int] -> [Int]
sieve (p:ns) = p : sieve [n | n <- ns, n `mod` p /= 0]

-- identify twin primes
twin :: (Int, Int) -> Bool
twin (x, y) = y == x + 2

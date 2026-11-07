{-# OPTIONS_GHC -Wno-tabs #-}
calcPi1, calcPi2 :: Int -> Double
calcPi1 n = sum (take n xs)
	where
		numerator = cycle [4, -4]
		denominator = [1, 3..]
		xs = zipWith (/) numerator denominator
calcPi2 0 = 0
calcPi2 n = 3 + sum (take (n - 1) xs)
	where
		numerator   = cycle [4, -4]
		denominator = [x * (x+1) * (x+2) | x <- [2, 4..]]
		xs = zipWith (/) numerator denominator

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x`mod`p/=0]

countTwins :: Integer -> Integer -> Int
countTwins f s = length [(a, b) | (a, b) <- zip primesbetween (drop 1 primesbetween), b - a == 2] -- smart shit that zips a list with its tail to get (n, n+1) tuples
	where
		primesbetween = dropWhile (< f) (takeWhile (< s) primes)

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y    = x : merge xs (y:ys)
    | x == y   = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))

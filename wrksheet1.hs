-- 2.1
classify :: Int -> String
classify n
    | n <= 9 = "failed"
    | n <=12 = "passed"
    | n <= 15 = "good"
    | n <= 18 = "very good"
    | n <= 20 = "excellent"

-- 2.2
classifyBMI :: Float -> Float -> String
classifyBMI w h
    | bmi < 18.5 = "underweight"
    | bmi < 25.0 = "normal weight"
    | bmi < 30.0 = "overweight"
    | bmi >= 30.0 = "obese"
    where bmi = w / (h*h)

-- 2.3
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = max (max x y) z
min3 x y z = min (min x y) z

-- 2.4
xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False

-- 2.5

safetail1 :: [a] -> [a]
safetail1 xs = if length xs == 0 then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 l  = tail l

safetail3 :: [a] -> [a]
safetail3 l
    | length l == 0 = []
    | otherwise = tail l

-- 2.6

short1 :: [a] -> Bool
short1 l = length l < 3

short2 :: [a] -> Bool
short2 []		= True
short2 [_]		= True
short2 [_,_]	= True
short2 _		= False

-- 2.7 median

median :: Ord a => a -> a -> a -> a
median x y z
  | (x >= y && x <= z) || (x >= z && x <= y) = x
  | (y >= x && y <= z) || (y >= z && y <= x) = y
  | otherwise                                = z

median2 :: (Num a, Ord a) => a -> a -> a -> a
median2 x y z = x + y + z - big - small
    where
        big   = max (max x y) z
        small = min (min x y) z

-- 2.8

propDivs :: Integer -> [Integer]
propDivs n = [x | x <- [1..n-1], rem n x == 0]

-- 2.9
perfects :: Integer -> [Integer]
perfects n = [x | x <- [2..n], sum (propDivs x) == x]

-- 2.10
pyths :: Integer -> [(Integer,Integer,Integer)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 2.11

isPrime :: Integer -> Bool
isPrime n = length [x | x <- [1..n], rem n x == 0] == 2

-- 2.12
myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _ <- [1..n]]

-- 2.13
-- (a)
binom :: Integer -> Integer -> Integer
binom n k = nf `div` (kf * nkf)
    where
      nf  = product [1..n]
      kf  = product [1..k]
      nkf = product [1..n-k]

-- (b)
pascal :: Integer -> [[Integer]]
pascal n = [[binom y z | z <- [0..y]] | y <- [0..n]]

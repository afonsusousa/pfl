-- 3.1
-- (a)
myand :: [Bool] -> Bool
myand []           = True
myand (True:rest)  = myand rest
myand (False:_)    = False

-- (b)
myor :: [Bool] -> Bool
myor [] = False
myor (True:_) = True
myor (False:rest) = myor rest

-- (c)
-- myconcat :: [[a]] -> [a]
-- myconcat []       = []
-- myconcat (xs:xss) = xs ++ myconcat xss

-- (d)
myreplicate :: Int -> a -> [a]
myreplicate n x
  | n <= 0    = []
  | otherwise = x : myreplicate (n - 1) x

-- (e)
myindex :: [a] -> Int -> a
myindex (x:_)  0 = x
myindex (_:xs) i = myindex xs (i - 1)

-- (f)
myelem :: Eq a => a -> [a] -> Bool
myelem _ []     = False
myelem e (x:xs) = e == x || myelem e xs

-- 3.3
intersperse :: a -> [a] -> [a]
intersperse a [] = []
intersperse a [elem] = [elem]
intersperse a (xs:xss) = xs : a : intersperse a xss

-- 3.4
nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub [y | y <- xs, y /= x]

-- 3.5
-- (a)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)
    | a <= x    = a : x : xs
    | otherwise = x : insert a xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- 3.6
-- (a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- (b)
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
    where
    half  = length xs `div` 2
    left  = take half xs
    right = drop half xs

-- 3.9
divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0) [1..n] -- \d is the anonymous input variable

-- 3.10
isPrimeFast :: Integer -> Bool
isPrimeFast n = n >= 2 && filter (\d -> n `mod` d == 0) [2..floor (sqrt (fromIntegral n))] == []

-- 3.11
myappend :: [a] -> [a] -> [a]      -- using foldr
myappend xs ys = foldr f z xs
    where f = (:)
          z = ys

myconcat :: [[a]] -> [a]           -- using foldr
myconcat xss = foldr f z xss
   where f = (++)
         z = []

myreverse :: [a] -> [a]            -- using foldr
myreverse xs = foldr f z xs
    where f = \x acc -> acc ++ [x]
          z = []

myreverse' :: [a] -> [a]           -- using foldl
myreverse' xs = foldl f z xs
    where f = \acc x -> x : acc
          z = []

myelem :: Eq a => a -> [a] -> Bool -- using any
myelem x ys = any f ys
    where f = undefined

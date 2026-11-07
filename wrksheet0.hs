-- 1.2
leftHalf, rightHalf :: [a] -> [a]
leftHalf l = take half l
    where half = length l `div` 2
rightHalf l = drop half l
    where half = length l `div` 2

-- 1.3
--	(a)

second :: [a] -> a
second l | length l >= 2 = head (tail l)

--	(b)

last :: [a] -> a
last l = head (reverse l)

-- last :: [a] -> a
-- last l = head (drop (length - 1) l)

--	(c)
init :: [a] -> [a]
init l = take (length l - 1) l

-- init :: [a] -> a
-- init l = reverse (drop 1 (reverse l))

--	 (d)

middle :: [a] -> a
middle l = head (drop (length l `div` 2) l)

-- (e)
checkPalindrome :: String -> Bool
checkPalindrome s = s == reverse s

-- 1.4
checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = a < (b + c) && b < (a + c) && c < (b + a) -- doesnt need those ()

-- 1.5
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

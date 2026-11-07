{-# OPTIONS_GHC -Wno-tabs #-}
fizzbuzz :: Int -> String
fizzbuzz x
    | x `mod` 15 == 0 = "Fizz Buzz"
    | x `mod` 3 == 0  = "Fizz"
    | x `mod` 5 == 0  = "Buzz"
    | otherwise       = show x

main :: IO ()
main = do
    n <- getLine
    mapM_ (putStrLn) [fizzbuzz x | x <- [1..read n]]

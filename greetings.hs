{-# OPTIONS_GHC -Wno-tabs #-}
main :: IO ()
main = do
	putStrLn "What is your name?"
	name <- getLine -- THIS IS A BIND, creates the variable
	putStrLn ("Hello, " ++ name ++ "! What is your age?")
	ageStr <- getLine
	let age = read ageStr :: Int
	putStrLn ("Next year you will be " ++ show (age + 1) ++ " years old. Good-bye!")

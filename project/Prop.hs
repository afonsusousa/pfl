{-
  A parser for propositions
  Based on the arithmetic expression parser in Calculator.hs
-}
module Prop where

import Parsing
import Data.Char

type Name = Char

-- | a data type for propositions
data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          deriving (Eq, Show)

-- | a parser for propositions
-- Grammar rules:
--
-- prop ::= term propCont
-- propCont ::= '|' term propCont | epsilon

-- term ::= factor termCont
-- termCont ::= '&' factor termCont | epsilon

-- factor ::= '!' factor | name | constant | '(' prop ')'
-- constant ::= 'T' | 'F'

prop :: Parser Prop
prop = do t <- term
          propCont t

-- the accumulator makes Or associate to the left
propCont :: Prop -> Parser Prop
propCont acc = do char '|'
                  t <- term
                  propCont (Or acc t)
               <|> return acc

term :: Parser Prop
term = do f <- factor
          termCont f

-- the accumulator makes And associate to the left
termCont :: Prop -> Parser Prop
termCont acc = do char '&'
                  f <- factor
                  termCont (And acc f)
               <|> return acc

factor :: Parser Prop
factor = do char '!'
            f <- factor
            return (Not f)
         <|>
         do x <- name
            return (Var x)
         <|>
         constant
         <|>
         do char '('
            p <- prop
            char ')'
            return p

name :: Parser Name
name = satisfy isLower

constant :: Parser Prop
constant = do char 'T'
              return (Const True)
           <|>
           do char 'F'
              return (Const False)

-- | parse a complete string; Nothing if it is not a valid proposition
parseProp :: String -> Maybe Prop
parseProp txt
  = case parse prop txt of
      [ (p, "") ] -> Just p
      _ -> Nothing

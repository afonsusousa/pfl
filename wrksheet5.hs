import Control.Arrow (ArrowChoice(left))
data Shape = Circle Float               -- radius
           | Rectangle Float Float      -- width, height
           | Triangle Float Float Float -- 3 sides

-- 5.1 (a)
area :: Shape -> Float
area (Circle r)       = pi * r^2
area (Rectangle w h)  = w * h
area (Triangle a b c) = sqrt (s * (s-a) * (s-b) * (s-c))
    where s = (a + b + c) / 2

-- 5.1 (b)
explain :: Shape -> String
explain (Circle r)       = "A circle with radius " ++ show r
explain (Rectangle w h)  = "A rectangle with " ++ show w ++ " by " ++ show h
explain (Triangle a b c) = "A triangle with sides " ++ show a ++ ", " ++ show b ++ " and " ++ show c

-- 5.4
-- (a)
data Set a = Empty                    -- empty set
           | Node a (Set a) (Set a)   -- value, left and right subtrees

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x == y = Node y left right
    | x < y = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

size :: Set a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right


height :: Set a -> Int                 -- count the height of the tree
height Empty = 0
height (Node _ left right)
	| lh > rh = 1 + lh
	| lh < rh = 1 + rh
	| otherwise = 1 + lh
	where
		lh = height left
		rh = height right

type Name = Char -- 'x', 'y', 'z', etc.
type Env = [(Name, Bool)]
data Prop = Const Bool
		| Var Name
		| Not Prop
        | And Prop Prop
	    | Or Prop Prop

vars :: Prop -> [Name]
vars (Const _) = []
vars (Var x)   = [x]
vars (Not p)   = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q)  = vars p ++ vars q

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans 1 = [[False], [True]]
booleans n | n > 0 = [ x ++ y | x <- booleans 1, y <- booleans (n - 1)]

eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x)
	= case lookup x env of
		Just b -> b
		Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q)
	= eval env p && eval env q
eval env (Or p q)
	= eval env p || eval env q

table :: Prop -> [(Env,Bool)]
table prop = [(env, (eval env prop)) | values <- booleans (length vs), let env = zip vs values]
	where vs = uniq (vars prop)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

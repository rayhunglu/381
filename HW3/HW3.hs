-- CS381, HW3
-- Name: 1. Han-Yu Wu
--       2. Po-Ying Chao
--       3. Jui-Hung Lu
--       4. Chi Wen
--       5. Chih-Hsiang Wang

module Homework3 where

{- Exercise 1. A Rank-Based Type Systems for the Stack Language -}
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int
         deriving Show

type Prog = [Cmd]
type Rank = Int
type CmdRank = (Int,Int)

-- (a)
rankC :: Cmd -> CmdRank
rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP i) = (i,0)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP xs = rank xs 0

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r | r < n = Nothing
              | otherwise = rank xs (r - n + m)
                where (n,m) = rankC x

-- (b)
type Stack = [Int]

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i) xs = (i:xs)
semCmd (ADD) (x:y:xs) = ((x+y):xs)
semCmd (MULT) (x:y:xs) = ((x*y):xs)
semCmd (DUP) (x:xs) = (x:x:xs)
semCmd (INC) (x:xs) = ((x+1):xs)
semCmd (SWAP) (x:y:xs) = (y:x:xs)
semCmd (POP i) xs = drop i xs

sem :: Prog -> Stack -> Stack
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

semStatTC :: Prog -> Maybe Stack
semStatTC xs = case rankP xs of
            Just r -> Just(sem xs [])
            _ -> Nothing

-- The new type of the function sem is type D = Stack -> Stack.
-- Because we check the type fisrt to avoid the error in later procedure,
-- we can exclude the use of maybe in function sem.


{- Exercise 2. Shape Language -}

-- (a)
data Shape = X
            | TD Shape Shape
            | LR Shape Shape
            deriving Show

type BBox = (Int,Int)

bbox :: Shape -> BBox
bbox X          = (1,1)
bbox (TD shX shY) = (max shXx shYx, shXy + shYy)
                    where (shXx,shXy) = bbox shX
                          (shYx,shYy) = bbox shY
bbox (LR shX shY) = (shXx + shYx, max shXy shYy)
                    where (shXx,shXy) = bbox shX
                          (shYx,shYy) = bbox shY

-- (b)
rect :: Shape -> Maybe BBox
rect X         = Just (1,1)
rect (TD shX shY) = case (rect shX, rect shY) of
                    (Just (shXx, shXy), Just (shYx, shYy)) -> if shXx == shYx
                          then Just(shXx, shXy + shYy) else Nothing
                    otherwise -> Nothing
rect (LR shX shY) = case (rect shX, rect shY) of
                    (Just (shXx, shXy), Just (shYx, shYy)) -> if shXy == shYy
                          then Just(shXx + shYx, shXy) else Nothing
                    otherwise -> Nothing


{- Exercise 3. Parametric Polymorphism -}
-- (a)
-- f x y = if null x then [y] else x
-- g x y=if not (null x) then [] else [y]
-- g [] y = []

-- (1)
-- type f = f :: [a] -> a -> [a]
-- type g = g :: [a] -> b -> [b]

-- (2)
-- Function f: In order to let both output x and [y] be the same type,
-- 			       the typy of x should be a list and y should be a value
-- Function g: Because the output is either [] or [y] and both of them didn't
--             reture x, the type of x and y can be separate

-- (3)
-- Function g is more general because x and y can be different types.
-- The function f should let x and [y] be the same type.

-- (4)
-- According to question2,
-- f returns x as an output, so y must also be of the type x but not in a
--    list, while g only returns an empty list or y in a list, so y does not
--    have to be the same type as x.

-- (b)
h :: [b] -> [(a, b)] -> [b]
h a b = a ++ snd (unzip b)

-- (c)
k :: (a -> b) -> ((a -> b) -> a) -> b
k a b = a (b (a))

-- (d)
-- No
-- Because we do not know the base case and the type of b.
-- In Haskell, we can only typy a through the meterials which are already
-- have deterministic type.

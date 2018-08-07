-- CS381, HW2
-- Name: 1. Han-Yu Wu
--       2. Po-Ying Chao
--       3. Jui-Hung Lu
--       4. Chi Wen
--       5. Chih-Hsiang Wang

module Homework2 where
import Data.Maybe
import System.IO
import Data.Maybe
import Data.List
import SVG (ppLines)
--it should runs with SVG.sh file

{- Q1 - A Stack Language -}

--Abstract syntax
type Prog = [Cmd]
-- type SavedMacros = [String]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show

--Type Definition
type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

--Semantic Definition
sem :: Prog -> D
sem [] a = a
sem (x:xs) a = sem xs (semCmd x a)

semCmd :: Cmd -> D
semCmd (LD n) a = case a of
               Just a -> if n < 0 then Nothing
                                  else Just (a ++ [n])
semCmd ADD    a = case a of
               Just a -> if length a < 2 then Nothing
                                         else Just ((init(init a))
                                                  ++ [last a + (last(init a))])
semCmd MULT   a = case a of
               Just a -> if length a < 2 then Nothing
                                         else Just ((init(init a))
                                                  ++ [last a * (last(init a))])
semCmd DUP    a = case a of
               Just a -> if length a < 1 then Nothing
                                         else Just (a ++ [(last a)])

-- Test
a  = Just []
t1= sem [LD 3, DUP, ADD, DUP, MULT] a
-- pass
t2= sem [LD 3, ADD] a
--fail


{- Q2 Extending the Stack Language by Macros -}

-- (a)
type Prog2 =[Cmd2]
data Cmd2 = LD2 Int
         | ADD2
         | MULT2
         | DUP2
         | DEF2 String Prog2
         | CALL2 String
         deriving Show

-- (b)
type Stack2 = [Int]
type Macros2 = [(String, Prog2)]
type D2 = Maybe (Stack2, Macros2) -> Maybe (Stack2, Macros2)

-- (c)
sem2 :: Prog2 -> D2
sem2 []     c = c
sem2 (o:os) c = sem2 os (semCmd2 o c)

semCmd2 :: Cmd2 -> D2

semCmd2 (LD2 x) (Just (xs, a)) = Just ((x:xs), a)
semCmd2 (LD2 _) _ = Nothing

semCmd2 ADD2 (Just ((x:y:xs), a)) = Just ((x+y:xs), a)
semCmd2 ADD2 _ = Nothing

semCmd2 MULT2 (Just ((x:y:xs), a)) = Just ((x*y:xs), a)
semCmd2 MULT2 _ = Nothing

semCmd2 DUP2 (Just ((x:xs), a)) = Just ((x:x:xs), a)
semCmd2 DUP2 _ = Nothing

semCmd2 (DEF2 n p) (Just (l, as)) = Just (l, (n,p):as)
semCmd2 (DEF2 _ _) _ = Nothing

semCmd2 (CALL2 n) (Just (l, as)) = case fD n as of
                                Just x    -> sem2 (snd x) (Just (l, as))
                                otherwise -> Nothing
semCmd2 (CALL2 _) _ = Nothing

fD:: String -> Macros2 -> Maybe (String, Prog2)
fD n as = find (\c -> fst c == n) as

l::Prog2
l = [LD2 2,LD2 3, DUP2,LD2 2]

-- Test
test2 :: Prog2 -> Maybe (Stack2, Macros2)
test2 l = sem2 l (Just ([], []))


{- Q3 Mini Logo -}

data Cmd3 = Pen Mode
              | MoveTo Int Int
              | Seq Cmd3 Cmd3
              deriving Show

data Mode = Up | Down
              deriving (Eq, Show)

type State = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)
type Lines = [Line]

-- Semantics
semS :: Cmd3 -> State -> (State, Lines)
semS (Pen m) (_,i,j) = ((m,i,j), [])
semS (MoveTo x y) (m,i,j)
                          | m == Up = ((m,x,y), [])
                          | m == Down = ((m,x,y), [(i,j,x,y)])
semS (Seq s1 s2) (m,i,j) = ((fst (semS s2 (fst (semS s1 (m,i,j))))),
                                         ((snd (semS s1 (m,i,j)))
                           ++ (snd (semS s2 (fst (semS s1 (m,i,j)))))))

sem' :: Cmd3 -> Lines
sem' a = snd (semS a (Up,0,0))

-- Test
square = Seq (Seq (Seq (Pen Down) (MoveTo 0 10)) (MoveTo 10 10))
        (Seq (MoveTo 10 0) (MoveTo 0 0))

test :: Cmd3 -> IO ()
test t = ppLines (sem' t)

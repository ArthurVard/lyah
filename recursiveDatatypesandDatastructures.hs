{--
http://www.cse.chalmers.se/edu/year/2012/course/TDA555/ex-week6.html

--}   

--Recursion and Datatypes

--1. (*) The Maximum Function
-- maxi x y returns the maximum of x and y

maxi :: (Ord a) => a -> a -> a
maxi a b | a <= b = b
		 | otherwise = a




--2. Sum of squares.
-- -- sumsq n returns 1*1 + 2*2 + ... + n*n
sumsq :: Int -> Int
sumsq n = sum $ map (\x ->x*x) [1..n]

--3. (*) The Towers of Hanoi

--hanoi :: Int -> Int
hanoi 1 = 1
hanoi n = 2 * hanoi (n-1) + 1









data Expr = Num Int | Add Expr Expr | Mul Expr Expr
				deriving (Show)


expr = Add (Add (Num 2) (Num 3)) (Mul (Num 4) (Num 5))
-- A. Define a function size :: Expr -> Int 
--	  that counts the number of operators in an expression.
size :: Expr -> Int
size (Num a) = 1
size (Add left right) = size left + size right
size (Mul left right) = size left + size right

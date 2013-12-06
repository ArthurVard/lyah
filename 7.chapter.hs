{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as Map  
  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)  

--example of a locker map


lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

lockerLookup :: Int -> LockerMap -> Either String Code  

lockerLookup lockerNumber locksmap =   
    case Map.lookup lockerNumber locksmap of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken 
         						then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"    
                        


--binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) 
				deriving (Show, Read, Eq)  

singleton :: a -> Tree a

singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a 

treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right) | x < y = Node y (treeInsert x left) right
                                 | x > y = Node x left (treeInsert x right)
                                 | x == y = Node y left right   

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False
treeElem x (Node y left right) | x == y = True
							   | x < y = treeElem x left
							   | otherwise = treeElem x right         

--treeSum :: Tree Int -> Int
treeSum EmptyTree = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

--use foldl to build tree form list
nums = [8,6,4,1,7,3,5]
tree = foldl (\acc  x -> treeInsert x acc ) EmptyTree nums


--about typeclasses

data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False

--make this an instance of Show by hand
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"     


instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


{--
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False  
--}    




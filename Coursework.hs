{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module Coursework where

import Data.List
import qualified Data.Set as HS (fromList, toList)
import Test.QuickCheck

{-
   PART 1.
   You need to define a Set datatype.
-}

-- you **MUST** change this to your own data type. The declaration of Set a =
-- Int is just to allow you to load the file into ghci without an error, it
-- cannot be used to represent a set.
data Set a = Null | Node (Set a) a (Set a) deriving (Show)

{-
   PART 2.
   If you do nothing else, you must get the toList, fromList and equality working. If they
   do not work properly, it is impossible to test your other functions, and you
   will fail the coursework!
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Ord a => Set a -> [a]
toList Null = []
toList (Node l val r) = toList l ++ (val : toList r)

-- fromList: do not forget to remove duplicates!
fromList :: Ord a => [a] -> Set a
fromList [] = Null
fromList (x:xs) = Node (fromList (filter (< x) xs)) x (fromList (filter (> x) xs))

-- Make sure you satisfy this property. If it fails, then all of the functions
-- on Part 3 will also fail their tests
toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  s1 == s2 = toList(s1) == toList(s2)

-- you should be able to satisfy this property quite easily
eqProp :: IO ()
eqProp =
  quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

{-
   PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
   TYPE SIGNATURES.
-}

-- the empty set
empty :: Set a
empty = Null

-- is it the empty set?
null :: Set a -> Bool
null Null = True 
null _ = False

-- build a one element Set
singleton :: a -> Set a
singleton x = Node (Null) x (Null)

-- insert an element *x* of type *a* into Set *s* make sure there are no
-- duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x Null = Node (Null) x (Null)
insert x (Node l val r) | x < val = Node (Coursework.insert x l) val r
                        | x > val = Node l val (Coursework.insert x r)
                        | x == val = Node l val r 

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union Null s = s
union s Null = s
union (Node l val r) s = Coursework.union l (Coursework.union r (Coursework.insert val s))

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Null Null = Null
intersection s Null = Null
intersection Null s = Null 
intersection (Node l1 val1 r1) (Node l2 val2 r2) | val1 == val2 = Node (intersection l1 l2) val1 (intersection r1 r2)
                                                 | val1 < val2 = Coursework.union (intersection (Node l1 val1 r1) l2) (intersection r1 (Node l2 val2 r2))
                                                 | val1 > val2 = Coursework.union (intersection (Node l1 val1 r1) r2) (intersection l1 (Node l2 val2 r2))

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference Null s = Null
difference s Null = s
difference (Node l val r) s | member val s = difference (Coursework.union l r) s 
                            | otherwise = Coursework.insert val (difference (Coursework.union l r) s)

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member val Null = False
member val1 (Node l val2 r) | val1 < val2 = member val1 l
                            | val1 > val2 = member val1 r
                            | otherwise = True

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality s = setfoldr (\_ acc -> acc + 1) s 0

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f Null = Null
setmap f s = setfoldr (Coursework.insert . f) s Null

-- right fold a Set using a function *f*
setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f Null acc = acc
setfoldr f (Node l val r) acc =  setfoldr f l (f val (setfoldr f r acc))

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Eq a) => a -> Set a -> Set a
removeSet x Null = Null
removeSet x (Node l val r) | x /= val = Node (removeSet x l) val (removeSet x r)
                           | x == val = merge l r
                               where merge Null right = right
                                     merge left Null = left
                                     merge left right = Node left (findMin right) (removeSet (findMin right) right)


findMin :: Set a -> a 
findMin (Node Null val r) = val
findMin (Node l val r) = findMin l

toListNO :: Set a -> [a]
toListNO Null = []
toListNO (Node l val r) = toListNO l ++ (val : toListNO r)

fromListNO :: [a] -> Set a
fromListNO [] = Null
fromListNO xs = Node (fromListNO leftList) midList (fromListNO rightList)
        where (leftList, midList:rightList) = midSplit xs
              midSplit ys = splitAt (div (length ys) 2) ys

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet Null = Coursework.singleton Null
powerSet (Node l val r) = fromListNO (map fromListNO aux)
        where aux = [] : foldr f [] (toListNO (Node l val r))
              f x acc = ([x] : (fmap (x:) acc)) ++ acc



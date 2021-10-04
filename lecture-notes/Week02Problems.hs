{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Problems where

import Week02

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function that counts the number of occurrences of an
      element in list: -}

popCount :: Eq a => a -> [a] -> Int
popCount x [] = 0
popCount x (y:ys)
      | x == y    = 1 + popCount x ys
      | otherwise = 0 + popCount x ys


{-    (popCount is short for "population count"). Examples:

         popCount 2 [1,2,5,2,7,2,9] == 3
         popCount 9 [1,2,5,2,7,2,9] == 1
         popCount 0 [1,2,5,2,7,2,9] == 0
-}


{- 2. Write a version of 'insert' that only inserts into a sorted list
      if the element is not already there. Examples:

         insertNoDup 2 [1,3,4]   == [1,2,3,4]
         insertNoDup 2 [1,2,3,4] == [1,2,3,4]
-}

insertNoDup :: Ord a => a -> [a] -> [a]
insertNoDup x []     = [x]
insertNoDup x (y:ys) = 
      case compare x y of
      EQ -> y:ys
      LT -> x:y:ys
      GT -> y:insertNoDup x ys


{- 3. Write a version of 'remove' that removes all copies of an element
      from a sorted list, not just the first one. Examples:

         removeAll 2 [1,2,2,3] == [1,3]
         removeAll 2 [1,3]     == [1,3]
-}

removeAll :: Ord a => a -> [a] -> [a]
removeAll x [] = []
removeAll x (y:ys) 
      | x == y    = removeAll x ys
      | otherwise = y : removeAll x ys


{- 4. Rewrite 'treeFind' and 'treeInsert' to use 'compare' and 'case'
      expressions. -}

treeFind2 :: Ord k => k -> KV k v -> Maybe v
treeFind2 k leaf                = Nothing
treeFind2 k (Node l (k', v') r) =
      case compare k k' of
            EQ -> Just v'
            LT -> treeFind2 k l
            GT -> treeFind2 k r      

treeInsert2 :: Ord k => k -> v -> KV k v -> KV k v
treeInsert2 k v Leaf             = Node Leaf (k, v) Leaf
treeInsert2 k v (Node l (k',v') r) =
      case compare k k' of
            EQ -> Node l (k,v) r
            LT -> Node (treeInsert2 k v l) (k',v') r
            GT -> Node l (k',v') (treeInsert2 k v r)


{- 5. MergeSort is another sorting algorithm that works in the following
      way:

      - If the list to be sorted is zero length, then it is already
        sorted.

      - If the list to be sorted has one element, then it is already
        sorted.

      - Otherwise, split the list into two, one with the even elements
        and one with the odd elements. Sort the two lists by calling
        'mergeSort' recursively. Then merge the two lists together
        maintaining the ordering.

      Write this function in three parts: -}

{-    'split' splits the input into two lists: one with the odd numbered
      elements and one with the even numbered elements. HINT: you can
      pattern match on multiple elements at the head of a list with
      'x1:x2:xs', and you can use the '(odds,evens) = ...' syntax in a
      'where' clause. -}

split :: [a] -> ([a], [a])
split [] = ([],[]) 
split [x] = ([x],[])
split (x1:x2:xs) = (odds,evens)
      where odds  = x1:fst(split xs)
            evens = x2:snd(split xs)

{-    'merge' merges two sorted lists into one sorted list. Examples:

          merge [1,3,5] [2,4,6]  = [1,2,3,4,5,6]
          merge [1,3,5] [7,9,11] = [1,3,5,7,9,11]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [x] [] = [x]
merge [] (x:xs) = x:xs
merge (x1:x2:xs) [] = x1:x2:xs
merge [x] [y] = case compare x y of
      EQ -> [x]++[y]
      LT -> [x]++[y]
      GT -> [y]++[x]
merge (x:xs) (y:ys) = case compare x y of
      EQ -> x:y:merge xs ys
      LT -> x:merge xs (y:ys)
      GT -> y:merge (x:xs) ys




{-    'mergeSort' uses 'split' and 'merge' to implement the merge sort
      algorithm described above. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
      where
            (x1, x2) = split xs

{- 6. Write another version of 'makeChange' that returns all the
      possible ways of making change as a list: -}

makeChangeAll :: [Coin] -> [Coin] -> Int -> [[Coin]]
makeChangeAll xs ys amount = [zs:z]
      where z = makeChange xs ys amount

{-
makeChange :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange coins        used 0 = Just used
makeChange []           used _ = Nothing
makeChange (coin:coins) used amount
  | amount >= coin =
    case makeChange coins (coin:used) (amount - coin) of
      Just coins -> Just coins
      Nothing    -> makeChange coins used amount
  | otherwise =
    makeChange coins used amount
-}

{- HINT: you don't need a case expression, just a way of appending two
   lists of possibilities. -}

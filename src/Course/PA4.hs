module Course.PA4 where

import Data.Array
import qualified Data.MemoCombinators as Memo
-- Algorithm ToolBox
-- no Memorization here
editDistance :: String -> String -> Int
editDistance str1 str2 =
  let
    n1 = length str1
    n2 = length str2
    funD i j =
      array ((0,0),(n1,n2)) [((ii,jj), funD' ii jj) | ii <- [0..n1], jj<-[0..n2]] ! (i, j)
      where
        funD' 0 j' = j'
        funD' i' 0 = i'
        funD' i' j' =
          let insertion = funD (i'-1) j' + 1
              deletion = funD i' (j'-1) + 1
              mismatch = funD (i'-1) (j'-1) + 1
              match = funD (i'-1) (j'-1)
          in if str1 !! (i'-1) == str2 !! (j'-1)
               then minimum [insertion, deletion, match]
               else minimum [insertion, deletion, mismatch]
  in funD n1 n2

-- use package
editDistance2 :: String -> String -> Int
editDistance2 str1 str2 =
  let
    n1 = length str1
    n2 = length str2
    funD :: Int -> Int -> Int
    funD = Memo.memo2 Memo.integral Memo.integral funD'
      where funD' 0 j = j
            funD' i 0 = i
            funD' i j =
              let insertion = funD (i-1) j + 1
                  deletion = funD i (j-1) + 1
                  mismatch= funD (i-1) (j-1) + 1
                  match = funD (i-1) (j-1)
              in if str1 !! (i-1) == str2 !! (j-1)
                    then minimum [insertion, deletion, match]
                    else minimum [insertion, deletion, mismatch]
  in funD n1 n2

-- implement by self
editDistance3 :: String -> String -> Int
editDistance3 str1 str2 = ar ! (n1, n2)
  where n1 = length str1
        n2 = length str2
        ar = listArray ((0,0),(n1,n2)) [f i j | i <- [0..n1], j <- [0..n2]]
        f 0 j = j
        f i 0 = i
        f i j =
          let insertion = ar ! (i-1, j) + 1
              deletion = ar ! (i, j-1) + 1
              mismatch = ar ! (i-1, j-1) + 1
              match = ar ! (i-1, j-1)
          in if str1 !! (i-1) == str2 !! (j-1)
                then minimum [insertion, deletion, match]
                else minimum [insertion, deletion, mismatch]

-- Knapsack Problem
knapsacksWithRepetition :: [(Integer, Integer)] -> Integer -> Integer
knapsacksWithRepetition knap cap = ar ! cap
  where ar = listArray (0, cap) (map f [0..cap])
        f 0 = 0
        f c =
          let change (wi, vi) = if c >= wi
                                then ar ! (c - wi) + vi
                                else 0
          in maximum (map change knap)

knapsacksWithoutRepetition :: [(Integer, Integer)] -> Integer -> Integer
knapsacksWithoutRepetition knap cap = ar ! (cap, n)
  where n = length knap
        ar = listArray ((0, 0), (cap, n)) (f <$> [0..cap] <*> [0..n])
        f 0 _ = 0
        f _ 0 = 0
        f c i =
          let old = ar ! (c, i-1)
              (wi, vi) = knap !! (i-1)
          in if c >= wi then max old (ar ! (c-wi, i-1) + vi)
                       else old

-- Algorithm Toolbox: Assignments
calculator :: Int -> (Int, [Int])
calculator n = ar ! n
  where ar = listArray (1, n) (f <$> [1..n])
        div2 m = if mod m 2 == 0 then div m 2 else m
        div3 m = if mod m 3 == 0 then div m 3 else m
        minus1 m = m - 1
        f 1 = (0, [1])
        f m = let (n', li') = minimum . map (ar !) . filter (< m) $ [div2 m, div3 m, minus1 m]
              in (n'+1, m:li')
-- 时间上为什么只是勉强及格?

-- Assignments2
optimalWeight :: [Int] -> Int -> Int
optimalWeight weight cap = ar ! (cap, n)
  where n = length weight
        ar = listArray ((0,0), (cap,n)) (step <$> [0..cap] <*> [0..n])
        step 0 _ = 0
        step _ 0 = 0
        step cap' i =
          let old = ar ! (cap', i-1)
              wi = weight !! (i-1)
          in if cap' >= wi then max old (ar ! (cap'-wi, i-1) + wi)
                           else old
-- TODO Longest common string in 3 strings

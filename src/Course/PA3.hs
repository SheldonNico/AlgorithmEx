module Course.PA3 where

import Data.Maybe (fromMaybe)
import Data.List (sort, minimumBy)
import Data.Array

-- Algorithm ToolBox
-- Sorting problem
slowsort :: Ord a => [a] -> [a]
slowsort xs =
  let step left (x:[]) = (x, left)
      step left (x:y:ys) = if x < y
        then step (left ++ [y]) (x:ys)
        else step (left ++ [x]) (y:ys)
  in if null xs
     then []
     else let (a, ys) = step [] xs
          in (a : slowsort ys)

-- Algorithm Toolbox: Assignments
binarySearch :: [Int] -> [Int] -> [Int]
binarySearch xs as =
  let n = length xs
      xs' = array (0, n-1) (zip [0..] . sort . zip xs $ [0..]) -- sort an array and keep the index in place
      lookup' :: Int -> Array Int (Int, Int) -> (Int, Int) -> Maybe Int
      lookup' a ar (low, high) =
        case compare low high of
          EQ -> if fst (ar ! low) == a then Just (snd (ar ! low)) else Nothing
          GT -> Nothing
          LT -> let mid = div (low + high) 2
                    (mid', ix) = ar ! mid
                in case compare mid' a of
                     EQ -> Just ix
                     LT -> lookup' a ar (mid + 1, high)
                     GT -> lookup' a ar (low, mid-1)
  in map (\x -> fromMaybe (-1) (lookup' x xs' (0, n-1))) as

countElements :: [Int] -> [(Int, Int)]
countElements xs =
  let xs' = sort xs
      cut _ [] = (1, [])
      cut y (a:as) =
        let (old, new) = cut y as
        in if y == a then (old+1, new)
                     else (1, a:as)
      helper [] out = out
      helper (b:bs) out = let (b', bs') = cut b bs
                          in helper bs' ((b, b') : out)
  in helper xs' []

majorElements :: [Int] -> Bool
majorElements [] = False
majorElements xs =
  let n = length xs
      test (_, a1) (_, a2) = compare a1 a2
  in (> div n 2) . snd . minimumBy test . countElements $ xs

sort1 :: Ord a => [a] -> [a]
sort1 [] = []
sort1 (x:xs) =
  let x1 = filter (<x) xs
      x2 = filter (>= x) xs
  in sort1 x1 ++ [x] ++ sort1 x2
-- this won't work when xs has duplicate elements

sort3 :: Ord a => [a] -> [a]
sort3 [] = []
sort3 (x:xs) =
  let x1 = filter (<x) xs
      x2 = filter (==x) xs
      x3 = filter (>x) xs
  in sort3 x1 ++ x : x2 ++ sort3 x3
-- this sort don't work will because `++` consume too much time.
-- use Data.Sequence replace List may get better speed

inversions :: [Int] -> ([Int], Int)
inversions xs =
  let ave =(`div` 2) . length $ xs
      (right, left) = splitAt ave xs
      merge [] z = (z, 0)
      merge y [] = (y, 0)
      merge y (z1:z2)=
        let (y1, y2) = span (<= z1) y
            (y3, num') = merge y2 z2
        in (y1 ++ (z1:y3), num' + length y2)
      -- merge (y1:y2) (z1:z2) out num = if y1 <= z1 then merge y2 (z1:z2) (out ++ [y1]) num
      --                                            else merge (y1:y2) z2 (out ++ [z1]) (num+1)
      (a1, b1) = inversions right
      (a2, b2) = inversions left
      (c1, c2) = merge a1 a2
  in if ave <= 0 then (xs, 0)
                else (c1, b1 + b2 + c2)

-- TODO Here

-- Algorithm

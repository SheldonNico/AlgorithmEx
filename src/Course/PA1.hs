-- | Source files for Algorithm Toolbox
module Course.PA1 where

import Data.List
import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.MemoCombinators as Memo

----------------------------------------------------------------------
----------------------- Week 2 ---------------------------------------
----------------------------------------------------------------------
-- Brackets
checkBrackets :: String -> Maybe Int
checkBrackets str =
  let out = brackets str
  in case out of
    [] -> Nothing
    ((x, _):_) -> if all (\(_, s) -> s `elem` "{[(") out
                       then Just x
                       else case dropWhile (\(_, s) -> s `elem` "{([") out of [] -> Nothing
                                                                              ((y, _):_) -> Just y

brackets :: String -> [(Int, Char)]
brackets str =
  let out = filter (\(_, x) -> x `elem` "{}[]()") (zip [1..] str)
      check a b = case (a, b) of
        ('{', '}') -> True
        ('[', ']') -> True
        ('(', ')') -> True
        _ -> False
      helper x [] = [x]
      helper xc@(_, x) acc@((_, a):ac) = if check x a then ac else xc:acc
  in foldr helper [] out



----------------------------------------------------------------------
----------------------- Week 3 ---------------------------------------
----------------------------------------------------------------------
-- Algorithms: Greedy Algorithms
minRefills :: [Int] -> Int -> Int
minRefills gasStations maxLen =
  let helper [] _ = 0
      helper [a] lastRef = if a - lastRef > maxLen then error "Not possible" else 0
      helper (cur:nex:zs) lastRef = if nex - lastRef > maxLen
                                        then 1 + helper (nex:zs) cur
                                        else helper (nex:zs) lastRef
  in helper gasStations 0

segments :: [Int] -> Int -> Int
segments [] _ = 0
segments xs len =
  let a:as = sort xs
      helper _ [] = 1
      helper a1 (x:xs) = if x-a1 <= len
                            then helper a1 xs
                            else 1 + helper x xs

  in helper a as
-- Knapsack problem
data Knapsack a b = Knapsack [(a, b, b)]
findBest :: (Eq a) => (Knapsack a Double) -> Double -> [(a, Int)]
findBest (Knapsack knap) cap =
  let knap' = sortBy (\(_, a1, b1) (_, a2, b2) -> compare (b1/a1) (b2/a2)) knap
      helper ((a, _, c) : xs) cap' = if cap' >= c
        then let tmp = div (round cap') (round c)
             in (a , tmp) : helper xs (cap' - (c * fromIntegral tmp))
        else helper xs cap'
      helper [] _ = []
  in helper knap' cap

knaptest :: Knapsack Char Double
knaptest = Knapsack [('a', 1.0, 2.0), ('b', 4.0, 4.0), ('c', 7.0, 2.0)]
-- Assignments
change :: Integer -> Integer
change n =
  let coins = [10, 5, 1]
      change1 [] _ = 0
      change1 (x : xs) m = div m x + change1 xs (mod m x)
  in change1 coins n

largestNumber :: [Integer] -> [Integer] -> Integer
largestNumber xs ys = sum $ zipWith (*) (sort xs) (sort ys)

unionpoints :: [(Integer, Integer)] -> (Integer, [Integer])
unionpoints seg =
  let seg' = sortBy (\(_, b1) (_, b2) -> compare b1 b2) seg
      step [] x = x
      step ((_, b) : bs) (n, seg'') = step (dropWhile ((<=b) . fst ) bs) (n+1, b : seg'')
  in step seg' (0, [])

summands :: Integer -> (Integer, [Integer])
summands m =
  let
    step :: Integer -> (Integer, [Integer]) -> (Integer, [Integer])
    step n (k, sub) = if n > 2*k then step (n-k) (k+1, k : sub)
                                 else (k, n : sub)
  in step m (1, [])

joinDigits :: [Integer] -> Integer
joinDigits =
  let test :: Integer -> Integer -> Ordering
      test a b = compare (show a ++ show b) (show b ++ show a)
  in read . concat . map show . sortBy (flip test)
-- actually, if you try to compare 23 and 3 in digit position to determine which
-- is bigger, it is much more complicate, while you can chose to determine a <+> b
-- and b <+> a, which is bigger.





----------------------------------------------------------------------
----------------------- Week 4 ---------------------------------------
----------------------------------------------------------------------
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




----------------------------------------------------------------------
----------------------- Week 5 ---------------------------------------
----------------------------------------------------------------------
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
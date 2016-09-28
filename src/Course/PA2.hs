module Course.PA2 where

import Data.List
import Data.Array.IArray
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



-- Algorithm: DataStructures
-- problem
-- how to solve those problem without mutation?!!

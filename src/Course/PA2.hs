module Course.PA2 where

import Data.List (sort)
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

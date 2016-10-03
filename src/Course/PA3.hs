-- | Source files for Algorithm Graphs
module Course.PA3 where

import qualified Data.Set as S
import Data.Array.IArray
-- Trees
-- sol1 子找父
{- treeHeight :: [Int] -> Int
treeHeight tree =
  let n = (length tree) - 1
      helper ind | ind == -1 = 1
                 | otherwise = 1 + helper (tree !! ind)
  in foldr (max . helper) 0 tree -}

-- sol2 父找子
treeHeight :: [Int] -> Int
treeHeight tree =
  let parseTree = zip tree [0..]
      helper n _ [] = n
      helper n old new = let (out1, out2) = foldr (\(ind, a) (x, y) -> if ind `elem` old then (a:x, y) else (x, (ind, a):y)) ([], []) new
                         in helper (n+1) out1 out2
  in helper 0 [-1] parseTree

-- sol3
-- data Tree a = Node a [Tree a] deriving (Show)
--
-- insertTree :: (Int, Int) -> Tree Int -> Tree Int
-- insertTree (n, x) (Node m xs) = if m == n then Node m (Node x []:xs)
--                                           else Node m (map (insertTree (n, x)) xs)
--
-- parseTree :: [Int] -> Tree Int
-- parseTree tree =
--   let ((_, a):as) = sort (zip tree [0..])
--       helper :: Int -> [(Int, Int)] -> Tree Int
--       helper n [] = Node n []
--       helper n xs = let out1 = filter (\(ind, _) -> ind == n) xs
--                         out2 = filter (\(ind, _) -> ind /= n) xs
--                         decompose (_, a) = helper a out2
--                     in Node n (map decompose out1)
--   in helper a as
--
-- heightTree :: Tree Int -> Int
-- heightTree (Node _ []) = 1
-- heightTree (Node _ xs) = foldr (\x acc -> max (heightTree x + 1) acc ) 0 xs
--
-- treeHeight :: [Int] -> Int
-- treeHeight = heightTree . parseTree

-- processing :: Int -> [(Int, Int)] -> [Int]
-- processing bufferSize sets =
--   let -- helper (timei, usagei) (acc, finishTime, storeLen)
--       helper (timei, usagei) ([], 0) = ([timei], [usagei])
--       helper (timei, usagei) (acc, finishTime, storeLen) =
--         let len = length finishTime
--         in if all (timei >) finishTime && len == bufferSize
--                then (-1:acc, finishTime)
--                else if all (timei >) finishTime && len < bufferSize
--                         then (biggest:acc, biggest+usagei:new)
--                         else
--
--
--         if any (timei >) finishTime then (finishTime:acc, )
--
--         if timei < finishTime && storeLen >= bufferSize
--             then (-1:acc, finishTime, storeLen)
--             else if timei < finishTime && storeLen < bufferSize
--                      then (finishTime:acc, finishTime+usagei, storeLen+1)
--                      else (timei:acc, timei+usagei, 0)
--       (out, _, _) = foldr helper ([], 0, 0) (reverse sets)
--   in reverse out






-- PA Grahs
-- PA1
type Vertex = Int
type NoDirEdge = (Vertex, Vertex)
type NoDirGraph = Array Vertex (S.Set Vertex)
buildNoDirG :: (Vertex, Vertex) -> [NoDirEdge] -> NoDirGraph
buildNoDirG bounds0 edges0 =
  accumArray (flip S.insert) S.empty bounds0 (edges0 ++ map swap edges0) where
    swap (a, b) = (b, a)
testGraph = buildNoDirG (1,4) [(1,2),(3,2)]

reach :: NoDirGraph -> Vertex -> Vertex -> Bool
reach gr u v =
  let getReach u xs =
        let subgr = filter (`notElem` xs) (S.toList $ gr ! u)
            out = subgr ++ xs
        in if null subgr then xs
                         else concatMap (flip getReach out) subgr
  in v `elem` (getReach u [])

reachbility :: (Vertex, Vertex) -> [NoDirEdge] -> (Vertex, Vertex) -> Bool
reachbility bounds0 edges0 (u, v) =
  reach (buildNoDirG bounds0 edges0) u v

-- PA2
-- components :: NoDirGraph -> Int
-- components gr =
--   let (a, b) = bounds gr
--       check _ _ [] n = n
--       check x store unprocessed n = -- check x if in xs
--           let now = gr ! x
--               store' = now ++ store
--               unprocessed' = filter (`notElem` store') unprocessed
--           in if null unprocessed' then n
--                  else map
--                    check (head unprocessed') store' unprocessed' (n+1)
--
--
--   in check a [] 0






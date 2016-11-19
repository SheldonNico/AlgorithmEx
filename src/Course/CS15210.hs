module Course.CS15210 where

import Control.Monad
import Data.List (permutations, tails, inits, foldl')
import Control.Arrow ((&&&))
import Data.Function (on)
import Text.Parsec
-------------------------------------------------------------------------------
--------------------------------- For CS15210 ---------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--------------------------------- Part II -------------------------------------
-------------------------------------------------------------------------------
-- Chapter 4 Fundamental
-- 4-1
-- a: O(n^log(2, 3)) b: O(n) c: O(n^2) d: O(n log(2, n)^2) e: O(n log(2, n)^2)
-- f: no boundary for this situation, suppose n = 2^k, and the boundary is 2
--    then the big o is O(k log(2, n))



-- Chapter 5 Genome sequencing
-- Problem
-- 5-1 brute-force
-- p501 ["ab", "cd", "xzcha", "ge"] "cdab"
p501 :: [String] -> String -> [String]
p501 xs str = 
  head . filter ((== str) . concat) . concatMap permutations . filterM (const [True, False]) $ xs
-- work: O(m^m) where m is the length of xs, which is the cost to generate permutations
-- span: O(m)
-- ??
-- 5-2 compact string concatenation
-- also consider this situaiton: ooo ooh -> ooooh or so no
p502 :: [String] -> String -> [[String]]
p502 xs str = 
  filter (elem str . removeOverlap) . concatMap permutations . filterM (const [True, False]) $ xs
  where -- removeOverlap 
    removeOverlap :: [String] -> [String]
    removeOverlap = foldr join [""]
      where join x acc = -- join "ooo" "ooh" = ["ooooh", "oooh"]
              acc >>= (\acc' -> 
              let prefixs = zip (inits acc') (tails acc')
              in map (\(_, rest) -> x ++ rest) .
                 filter (\(pre, _) -> reverse (take (length pre) (reverse x)) == pre)
                 $ prefixs)
-- 5-3
-- find in the textbook
-- 5-4
-- find in the textbook
-- 5-5 check whether s is substring of t
p505 :: String -> String -> Bool
p505 s t | length s > length t = False
         | otherwise = 
           let len = length s
           in any ((==s) . take len) (tails t)
-- 5-6
-- reason: any permutations reresent a set of ordered paths, which contain n-1 directed path
-- which connect all vertices exact once and add the start and end vertex will get the final
-- hamiltonian n paths
-- 5-7
-- the problem: hamiltonian cycle that starts and ends at a specified <=>
-- the problem: hamiltonian cycle that starts at any points
-- 5-8
-- easy: if we contain xy = join x y in our list, then 
-- overlap (xy, y) > overlap(x, y) >= overlap(any other pair))
-- the `>` is strict, because they are snippets, so length xy > length x or length y
-- so it will be removed in the first recursion, then there is no need to do that step
-- 5-9
-- easy: every step, xy contain x and y, and we remove x and y
-- so in the final, we get a string contain every element of the list
-- 5-10 ??
-- ["abcde", "bcdef", "cdef", "efg"]
-- 5-11 ??
-- 5-12
-- a. brute force: try all posible permutations of items, filter out the combination reach
-- out the scale and the select the maximum value combination
-- b. work O(2^n) + O(2^n) + O(2^n) = O(2^n)
--    span ??
-- c. greedy algorithm: 
--    1st: calculate every item : value / weight
--    2nd: first select the arg max value/weight, item_i;
--    3th: recursion solve algorithm(weight - weight_i, items)
-- d. yes
-- e. work O(n^2)
--    span ??

-- Assignment
-- parenlab
data Paren = OParen | CParen deriving (Eq, Show)
type Parens = [Paren]

runparen :: (Parens -> Maybe Int) -> String -> Either ParseError (Maybe Int)
runparen pf str = 
  let parseC = const CParen <$> char ')'
      parseO = const OParen <$> char '('
      parseP = many (parseC <|> parseO)
  in pf <$> (parse parseP "" str)

isClose []          = True
isClose (CParen:_)  = False
isClose (OParen:xs) =
  let check [] zs                   = isClose zs
      check _ []                    = False
      check (OParen:ys) (CParen:zs) = check ys zs
      check ys (OParen:zs)          = check (OParen : ys) zs
  in check [OParen] xs

isMatched []       = False
isMatched [_]      = False
isMatched (x : xs) =
  x == OParen && (last xs) == CParen && isClose (init xs)

parenDistSeq :: Parens -> Maybe Int
parenDistSeq parens =
  let
      pd (opens, maxState)   (i, OParen) = (i:opens, maxState)
      pd ([], maxState)      (_, CParen) = ([], maxState)
      pd (j:opens, maxState) (i, CParen) = (opens, eitherMaybe max (Just (i-j+1)) maxState)
      eitherMaybe f (Just x) (Just y)    = Just (f x y)
      eitherMaybe _ (Just x) Nothing     = Just x
      eitherMaybe _ Nothing (Just y)     = Just y
  in snd $ foldl' pd ([], Nothing) (zip [0..] parens)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
parenDistBrut :: Parens -> Maybe Int
parenDistBrut parens = 
  let len = length parens
      out = [ j - i + 1 | i <- [0..len],
              j <- [i+1..len-1],
              let str = slice i j parens,
              isMatched str]
  in if null out then Nothing else Just (maximum out)

-- convert from the solution provided
parenDistDivCon :: Parens -> Maybe Int
parenDistDivCon parens = 
  let
    pd :: Parens -> ([Int], [Int], Maybe Int, Int)
    pd [OParen] = ([0], [], Nothing, 1)
    pd [CParen] = ([], [0], Nothing, 1)
    pd xs = 
      let (left, right) = splitAt (length xs `div` 2) xs
          (pl, sl, ol, ll) = pd left
          (pr', sr', or, lr) = pd right
          pr = (+ll) <$> pr'
          sr = (+ll) <$> sr'
          lenl = length pl
          lenr = length sr
          pn = if lenl >= lenr then take (lenl - lenr) pl ++ pr else pr
          sn = if lenl <= lenr then sl ++ (drop lenl sr) else sl
          on = if null pn && null sn 
                 then if null pl || null sr
                        then max <$> ol <*> ol
                        else Just (ll-head pl + last sr' + 1) 
                 else Nothing
          ln = ll + lr
      in (pn, sn, on, ln)
  in case pd parens of
       (_, _, out, _) -> out



-- Chapter 6 Sequence
-- Course materials
-- example 1: primes
primesBF n = filter isPrimes [2..n]
  where isPrimes m = length [j | i <- [1..floor (sqrt (fromIntegral m))],
                                 let j = mod m i,
                                 j == 0] == 1

primesSieve n = filter isPrimes [2..n]
  where sieve = [i*j | i <- [2..floor (sqrt (fromIntegral n))], j <- [1..div n i]]
        isPrimes m = m `notElem` sieve
-- this implementation is slow because list in haskell do slow in elem/update
-- problem
-- p601
-- subseq a i j = tabulate (\ind -> a[ind + i]) a (j-i)
-- p602 solve parenDist
p602 :: Parens -> Bool
p602 xs = foldl' reduce (Just 0) xs == Just 0
  where reduce Nothing _ = Nothing
        reduce (Just n) CParen = if n == 0 then Nothing else Just (n-1)
        reduce (Just n) OParen = Just (n+1)
-- p603 proof reduce is equivalent to iterate

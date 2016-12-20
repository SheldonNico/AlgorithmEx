module Course.CS15210 where

import Control.Monad
import Data.List (permutations, tails, inits, foldl', scanl', sort)
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
--
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
-- proof is easy
-- iterate f id a = id `f` a[0] `f` a[1] `f` a[2] ... `f` a[n-1] `f` a[n]
-- because of associty law
-- it equals to = (id `f` a[0] `f` a[1] `f` ... `f` a[floor (div n 2) - 1]) `f`
--                (id `f` a[floor (div n 2)] `f` ... `f` a[n-1] `f` a[n])
--              = reduce f id a
-- p604 solve match paren with scanl' instead of foldl'
p604 :: Parens -> Bool
p604 xs = all (>=0) out && last out == 0
 where  out = scanl' reduce 0 xs
        reduce n CParen = n-1
        reduce n OParen = n+1
-- p605 
-- map f xs = tabulate (\i -> f xs[i]) |xs|
-- for simplify: suppose f take O(1) work and span
-- cost in array sequence: Work-O(n) Span-O(1)
-- cost in tree sequence: Work-O(nlog(n)) Span-O(log(n))



-- Chapter 7 Algorithm-Design Tech: Contraction
-- example
reduce_contract :: (a -> a -> a) -> [a] -> a
reduce_contract _ [a] = a
reduce_contract f as  = 
  -- import: assumption |as| is a power of two.
  let bs = [f (as !! i) (as !! (i+1)) | i <- [0,2 .. length as-1]] 
  in reduce_contract f bs

-- p701 find rank r in a list
-- just pusedo code
-- step1: div list into groups, every group have r+1 elements, and sort every group: work-O(n/r) * O(r)
-- step2: join every nearby two group, get new group of r+1-elements: work-W(n/r) = W(n/r * 1/2) + O(n/r * 1/2) * O(r * const)
-- step3: at last, only 1 group remains, take the last member: Const
-- suppose sort every r+1 elements have const time, then work-O(n/r) * O(r)
p701 :: Ord a => Int -> [a] -> a
p701 r xs = last . foldr1 (\x acc -> take r (join2 x acc)) . map sortr . group r $ xs
  where
    sortr :: Ord a => [a] -> [a]
    sortr = sort
    join2 :: Ord a => [a] -> [a] -> [a]
    join2 [] ys = ys
    join2 (x:xs) ys =
      let (as, bs) = span (<= x) ys
      in as ++ [x] ++ join2 xs bs
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group r xs = take r xs : group r (drop r xs)

-- Chapter 9 Maximum continuous sum
mcsBF :: (Num a, Ord a) => [a] -> (a, [a])
mcsBF xs =
  let n = length xs
  in maximum [ (sum out, out) | i <- [0..n-1], j <- [i..n-1], let out = slice i j xs ]

mcsBF2 :: (Num a, Ord a) => [a] -> a
mcsBF2 xs =
  let n = length xs
      sublist = take n $ iterate (drop 1) xs
  in maximum . map (maximum . scanl (+) 0) $ sublist

mcsScan :: [Int] -> Int
mcsScan xs = 
  let c = scanl (+) 0 xs
      d = scanl min maxBound c
      e = zipWith (-) c d
   in foldl max minBound (tail e)

mcsDivAndCon' :: [Int] -> (Int, Int, Int, Int)
mcsDivAndCon' [] = (minBound, minBound, minBound, 0)
mcsDivAndCon' [a] = (a, a, a, a)
mcsDivAndCon' xs =
  let (right, left) = splitAt (div (length xs) 2) xs
      (p1, m1, s1, t1) = mcsDivAndCon' right
      (p2, m2, s2, t2) = mcsDivAndCon' left
  in (max (t1+p2) p1, maximum [m1, m2, s1+p2], max s2 (t2+s1), t1+t2)
mcsDivAndCon :: [Int] -> Int
mcsDivAndCon xs = 
  let (_, m, _, _) = mcsDivAndCon' xs
  in m

mcsDivAndCon0 :: [Int] -> Int
mcsDivAndCon0 [] = minBound
mcsDivAndCon0 [a] | a > 0 = a
                  | otherwise = 0
mcsDivAndCon0 xs =
  let (right, left) = splitAt (div (length xs) 2) xs
      mr = mcsDivAndCon0 right
      ml = mcsDivAndCon0 left
      maxRight = maximum . scanr (+) 0
      maxLeft = maximum . scanl (+) 0
  in maximum [mr, ml, maxRight right + maxLeft left]

-------------------------------------------------------------------------------
--------------------------------- Part III ------------------------------------
-------------------------------------------------------------------------------
-- Chapter 10 Probablity Theory
-- p1001:
-- a. A \belong B, P(B) = P(A) + P(B / A) >= P(A)
-- b. A \union B = A + B + (B \intersect A), all 3 parts disjoint
-- c. P(A \union B) = P(A) + P(B) + P(B \intersect A) <= P(A) + P(B)
-- p1002:
-- a. P(A \union B) <= P(A) + P(B)
-- b. P(A \union B \union C) <= P(A) + P(B \union C) <= P(A) + P(B) + P(C)
-- p1003:
-- P(A | B) = P(A \intersect B) / P(B)
-- a. P(A \intersect B) <= P(B), so 1st law satisfy
-- b. P(A | B \union C | B) = P(A \union C | B) = P((A \union C) \intersect B) / P(B)
--                          = P((A \intersect B) \union (C \intersect B)) / P(B) (since sets property)
--                          = P(A \intersect B) + P(C \intersect B) / P(B) (sets property)
-- c. P(\Omega | B) = P(Omega \intersect B) / P(B) = P(B)/P(B) = 1
-- p1004
-- no, this example: roll a coin, A is the front side, and B is the other side.
-- A and B are surely disjoint, but not independent.
-- because A happens, then B mustn't happen

-- Chapter 11 Randomized Algorithms

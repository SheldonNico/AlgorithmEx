import Course.PA1
import System.Directory
import System.IO
import Data.List (sort)

{- PA1
main :: IO ()
main = do
    test <- listDirectory "test\\PA1tests1"
    let dir = sort . map (\y -> "test\\PA1tests1\\" ++ y ) . filter (\s -> '.' `notElem` s) $ test
    out <- mapM checkFile dir
    mapM_ putStrLn out

checkFile :: FilePath -> IO String
checkFile filepath = do
    cont1 <- readFile filepath
    cont2 <- readFile (filepath ++ ".a")
    let out = case checkBrackets cont1 of Nothing -> if cont2 == "Success\n" then "Right for " ++ filepath
                                                                             else "Wrong for " ++ filepath ++ ":" ++ (take 20 cont1)
                                          Just ind -> if cont2 == show ind ++ "\n" then "Right for " ++ filepath
                                                                                   else "Wrong for " ++ filepath ++ ":" ++ (take 20 cont1 )
    return out
-}

{- PA2
main :: IO ()
main = do
  test <- listDirectory "test\\PA1tests2"
  let dir = sort . map (\y -> "test\\PA1tests2\\" ++ y ) . filter (\s -> '.' `notElem` s) $ test
  out <- mapM checkFile dir
  mapM_ putStrLn out

checkFile :: FilePath -> IO String
checkFile filepath = do
  cont1 <- readFile filepath
  cont2 <- readFile (filepath ++ ".a")
  let (_:xs:_) = lines cont1
      ns = [read x | x <- (words xs)]
      out = if cont2 == show (treeHeight ns) ++ "\n" then "Right for " ++ filepath
                                                     else "Wrong for " ++ filepath ++ ":" ++ (take 20 cont1)
  return out
-}

-- PA3
{- main :: IO ()
main = do
  test <- listDirectory "test\\PA1tests3"
  let dir = sort . map (\y -> "test\\PA1tests3\\" ++ y ) . filter (\s -> '.' `notElem` s) $ test
  out <- mapM checkFile dir
  mapM_ putStrLn out

checkFile :: FilePath -> IO String
checkFile filepath = do
  cont1 <- readFile filepath
  cont2 <- readFile (filepath ++ ".a")
  let (x:xs) = lines cont1
      bufferSize = read . head . words $ x
      input = [(read a, read b) | [a, b] <- map words xs]
      out = if cont2 == unlines (map show $ processing bufferSize input)
                then "Right for " ++ filepath
                else "Wrong for " ++ filepath ++ ":" ++ take 20 cont1
  return out -}

-- Graphs PA1
main :: IO ()
main = do
  let samples = [(4, [(1,2),(3,2)], 1, 4)]
      check (a, test, u, v) =
        putStrLn (show test ++ "\nAnswers: " ++ show (reachbility (0, a) test (u, v)))
  mapM_ check samples

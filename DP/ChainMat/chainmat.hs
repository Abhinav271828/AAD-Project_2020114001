import System.Environment
import Data.List
import Data.Ord

-- Find Minimum Cost --
cost :: [(Int,Int)] -> Int
cost m = last $ (costs m) !! 0

costs :: [(Int,Int)] -> [[Int]]
costs m = c
  where c = [ 0:
                [
                 minimum [ (c !! i !! (k-i)) + 
                           (c !! (k+1) !! (j-k-1)) + 
                           ((fst $ m !! i) * 
                                (snd $ m !! k) *
                                    (snd $ m !! j)) | k <- [i..j-1] ]
                  | j <- [i+1..n-1] ]
                  | i <- [0..n-2]   ] ++ [[0]]
        n = length m
-----------------------

-- Find Chain --
data Tree a = Node (Tree a) (Tree a) | Leaf a

instance Show a => Show (Tree a) where
  show (Node t1 t2) = "(" ++ (show t1) ++ " x " ++ (show t2) ++ ")"
  show (Leaf t)     = show t

chain :: [(Int,Int)] -> (Int, Tree (Int,Int))
chain m = last $ (chains m) !! 0

chains :: [(Int,Int)] -> [[(Int, Tree (Int,Int))]]
chains m = c
  where c = [ (0, Leaf (m !! i)) :
                 [
                    extend i j
                   | j <- [i+1..n-1] ]
                   | i <- [0..n-2]   ] ++ [[(0, Leaf (last m))]]
        n = length m
        extend i j = let k = head $ sortBy (comparing
                                           (getCost i j))
                                           [i..j-1]
                     in (getCost i j k,
                           Node (snd $ c !! i !! (k-i))
                                (snd $ c !! (k+1) !! (j-k-1)))
        getCost i j k = (fst $ c !! i !! (k-i)) +
                        (fst $ c !! (k+1) !! (j-k-1)) +
                        ((fst $ m !! i) * 
                             (snd $ m !! k) *
                                 (snd $ m !! j))
----------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: [(Int,Int)]
          -- print (cost i)
          print (chain i)
----------

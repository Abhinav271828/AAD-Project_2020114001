import System.Environment
import Data.IntSet

data Tree = Leaf Int | Node Int [Tree] deriving (Show, Read)

-- Size of Maximal Independent Set --
maxi :: Tree -> Int
maxi (Leaf x) = 1
maxi t@(Node n cs) = max (1 + sum (Prelude.map maxi $ gchildren t))
                         (sum (Prelude.map maxi cs))

gchildren :: Tree -> [Tree]
gchildren (Leaf _) = []
gchildren (Node n cs) = cs >>= children
                           where children (Leaf _) = []
                                 children (Node n' cs') = cs'
-------------------------------------

-- Maximal Independent Set --
maxiset :: Tree -> IntSet
maxiset (Leaf x) = singleton x
maxiset t@(Node n cs) = maxByS (insert n $
                                  unions (Prelude.map maxiset $
                                               gchildren t))
                               (unions (Prelude.map maxiset cs))
                       where maxByS s1 s2 = if (size s1 >= size s2)
                                            then s1 else s2
-----------------------------

-- Size of Maximal Independent Set (DP) --
maxi' :: Tree -> Int
maxi' t = list t (highest t) !! 0

highest :: Tree -> Int
highest (Leaf x) = x
highest (Node n cs) = maximum $ n : (Prelude.map highest cs)

list :: Tree -> Int -> [Int]
list (Leaf x) k = insertAt 1 x (replicate (k+1) 0)
list t@(Node n cs) k = let csets = [list c k | c <- cs]
                           overall = Prelude.foldr (zipWith (+))
                                                   (replicate (k+1) 0)
                                                   csets
                       in insertAt (max 
                                     (1 + sum [overall !! g | g <- gch t])
                                     (sum [overall !! c | c <- ch t]))
                                   n overall

ch :: Tree -> [Int]
ch (Leaf _) = []
ch (Node _ cs) = Prelude.map getVal cs
                   where getVal (Leaf x) = x
                         getVal (Node n _) = n

gch :: Tree -> [Int]
gch (Leaf _) = []
gch (Node n cs) = cs >>= ch

insertAt :: a -> Int -> [a] -> [a]
insertAt k i ks = let (before, a:after) = splitAt i ks
                  in before ++ [k] ++ after
------------------------------------------

-- Maximal Independent Set (DP) --
maxiset' :: Tree -> IntSet
maxiset' t = list' t (highest t) !! 0

list' :: Tree -> Int -> [IntSet]
list' (Leaf x) k = insertAt (singleton x) x (replicate (k+1) empty)
list' t@(Node n cs) k = let csets = [list' c k | c <- cs]
                            overall = Prelude.foldr (zipWith union)
                                                    (replicate (k+1) empty)
                                                    csets
                        in insertAt (maxByS
                                       (insert n $
                                           unions [overall !! g | g <- gch t])
                                       (unions [overall !! c | c <- ch t]))
                                     n overall
                            where maxByS s1 s2 = if (size s1 >= size s2)
                                                 then s1 else s2
----------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: Tree
          -- print (maxi i)
          -- print (maxiset i)
          print (maxi' i)
          -- print (maxiset' i)
----------

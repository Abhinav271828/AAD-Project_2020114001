import System.Environment
import Data.IntSet hiding (map, filter)

type Vertex = Int -- type synonym
data Edge = E Int Int Int -- new datatype (source, dest, weight)
            deriving (Eq,Show,Read)

instance Ord Edge where -- defining an ordering on edges (by weight)
  (E _ _ w1) < (E _ _ w2) = w1 < w2
  compare (E _ _ w1) (E _ _ w2) = compare w1 w2

type Graph = ([Vertex],[Edge])

-- Kruskal's Algorithm --
getMST :: Graph -> Graph -- wrapper function
getMST g@(v,e) = getMST' ((length v) - 1) 0 [] g

getMST' :: Int -> Int -> [Edge] -> Graph -> Graph -- n == |v|, m = |es|
getMST' n m es (v,e) = if (m == n) -- tree complete
                     then (v,es)
                     else getMST' n (m+1) ((minimum (filter (\x -> (adjTo es x) && -- least weight elmt
                                                        (x `notElem` es)) -- not forming cycle
                                                   e)) : es) (v,e)

adjTo :: [Edge] -> Edge -> Bool -- checks for adjacency and cycles
adjTo [] _         = True
adjTo es (E s d _) = (adjD == []) /= (adjS == []) -- if both sides adjacent, cycle formed
                        where adjS = filter (\(E x y _) ->
                                                 (x == s) || (y == s)) es -- one side touching
                              adjD = filter (\(E x y _) ->
                                                 (x == d) || (y == d)) es -- other side touching
-------------------------

-- Disjoint Set Implementation --
getMST2 :: Graph -> Graph -- wrapper function
getMST2 (v,e) = (v, getSet verts [] (sort [] e))
                  where verts = map singleton v
                        sort left []  = left
                        sort left (x:xs) = sort (insert x left) xs
                        insert x l = (takeWhile (<x) l) ++ x:(dropWhile (<x) l)

getSet :: [IntSet] -> [Edge] -> [Edge] -> [Edge] -- finds list of edges forming tree
getSet _ x []               = x
getSet a x (e@(E s d w):es) = let setS = findr s a
                                  setD = findr d a
                              in if (setS /= setD) -- sets are not the same
                                   then getSet (unionIn setS setD a) (e:x) es -- add to list
                                   else getSet a x es

findr :: Int -> [IntSet] -> IntSet -- get the set
findr x a = if (r == []) then empty else (head r)
                where r = filter (member x) a

unionIn :: IntSet -> IntSet -> [IntSet] -> [IntSet] -- union in the list of sets
unionIn x y a = map (\s -> if (s == x)
                             then (union x y)
                             else s) (filter (/= y) a)
---------------------------------

-- Disjoint Set Implementation (mergesort) --
getMST3 :: Graph -> Graph -- wrapper function
getMST3 (v,e) = (v, getSet verts [] (mergesort e))
                  where verts = map singleton v

merge :: Ord a => [a] -> [a] -> [a]
merge x        []       = x
merge []       y        = y
merge a@(x:xs) b@(y:ys) = if x < y
                             then x:(merge xs b)
                             else y:(merge a ys)

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort left) (mergesort right)
                   where (left,right) = splitAt ((length xs)
                                                  `div` 2) xs
---------------------------------
-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: Graph
          -- print (getMST i)
          -- print (getMST2 i)
          print (getMST3 i)
----------

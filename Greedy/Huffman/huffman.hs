import System.Environment
import Data.Ord
import Data.List

-- Huffman Encoding --
data Tree a = Node (Tree a) (Tree a) | Leaf a -- values only at leaves
                  deriving (Show, Eq)

huffman :: [(String, Float)] -> [(String, Float, String)] -- builds tree and generates code
huffman cs = trav $ getTree [(Leaf (a,b), b) | (a,b) <- cs]

getTree :: Eq a => [(Tree a, Float)] -> Tree a -- a = (String, Float); builds tree
getTree [(t,_)] = t
getTree ts      = getTree $ map (\t -> if t == min1             -- merge smallest two
                                   then (Node (fst t) (fst min2),
                                              (snd min1) + (snd min2))
                                   else t) (filter (/= min2) ts)
                     where min1 = head $ sortBy (comparing snd) ts
                           min2 = head $ sortBy (comparing snd) (filter (/= min1) ts)

trav :: Eq a => Tree (a,b) -> [(a, b, String)] -- generates code
trav (Node t1 t2) = [ (w,f,'0':c) | (w,f,c) <- trav t1 ] ++ -- internal node
                    [ (w,f,'1':c) | (w,f,c) <- trav t2 ]
trav (Leaf (w,f)) = [ (w,f,"") ]                            -- leaf node

expL :: [(String, Float, String)] -> Float -- expected length; weighted mean
expL cs = (sum (map (\(_,f,c) -> (length c) `mul` f) cs)) /
                (sum (map freq cs))
             where mul x y = (fromIntegral x :: Float) * y
                   freq (_,f,_) = f
----------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: [(String, Float)]
          print (huffman i)
          -- print (expL (huffman i))
----------

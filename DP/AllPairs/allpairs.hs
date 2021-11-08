import System.Environment

type Vertex = Int
type Edge = ((Int,Int),Float)
type Graph = ([Vertex],[Edge])

-- All Pairs Shortest Path Lengths --
lengths :: Graph -> [[Float]]
lengths = (map (map last)) . lengths'

lengths' :: Graph -> [[[Float]]]
lengths' g@(vs,es) = p
        where p = [ [ if (i == j) then [ 0 | _ <- vs ]
                      else
                      ( min ((lookup' (i,0) es) + (lookup' (0,j) es))
                            (lookup' (i,j) es) ) :
                          [ min ((p !! i !! k !! (k-1)) +
                                     (p !! k !! j !! (k-1)))
                                 (p !! i !! j !! (k-1))
                                | k <- tail vs ]
                            | j <- vs ]
                            | i <- vs ]
              lookup' k kvs = case (lookup k kvs) of
                                Just v -> v
                                Nothing -> (1/0)
-------------------------------------

-- All Pairs Shortest Paths --
paths :: Graph -> [[(Float, [Edge])]]
paths = (map (map last)) . paths'

paths' :: Graph -> [[[(Float, [Edge])]]]
paths' g@(vs,es) = p
        where p = [ [ if (i == j) then [ (0,[]) | _ <- vs ]
                      else
                      ( minByF (lookup' (i,j) es)
                               (join (lookup' (i,0) es)
                                     (lookup' (0,j) es))) :
                          [ minByF (p !! i !! j !! (k-1))
                                   (join (p !! i !! k !! (k-1))
                                         (p !! k !! j !! (k-1)))
                                | k <- tail vs ]
                            | j <- vs ]
                            | i <- vs ]
              join (l1, p1) (l2, p2) = if (l1 == (1/0) || l2 == (1/0))
                                       then ((1/0), [])
                                       else (l1 + l2, p1 ++ p2)
              minByF a@(a1,_) b@(b1,_) = if (a1 <= b1) then a else b
              lookup' k kvs = case (lookup k kvs) of
                                Just v -> (v, [(k,v) :: Edge])
                                Nothing -> ((1/0), [])
------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: Graph
          -- print (lengths i)
          print (paths i)
----------

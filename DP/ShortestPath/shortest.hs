import System.Environment
import Data.Ord
import Data.List

type Vertex = Int
type Edge   = ((Vertex, Vertex), Int) -- ((source, dest), weight)
type Graph  = ([Vertex],[Edge])

getS :: Edge -> Vertex
getS ((s,_),_) = s
getD :: Edge -> Vertex
getD ((_,d),_) = d

-- Toposort --
toposort :: Graph -> [Vertex] -- wrapper function
toposort (vs,es) = fst $ foldl (flip $ topo es)
                               ([], [False | _ <- vs])
                               vs

topo :: [Edge] -> Vertex -> ([Vertex], [Bool]) -> ([Vertex],[Bool])
  -- applied on each vertex; pushes to stack and changes visited array
topo es v (stack, vis) = if (vis !! v)
                         then (stack, vis)
                         else (\(a,b) -> (v:a,b)) $
                            foldr (topo es)
                                  (stack, vis')
                                  (adjs)
                                where adjs = map getD -- all vertices v->u
                                                 (filter
                                                    ((== v) . getS) es)
                                      vis' = (take v vis) ++ [True]
                                             ++ (drop (v+1) vis)
--------------

-- Shortest Distance between Two Vertices --
getDist :: Graph -> Vertex -> Vertex -> Int -- wrapper function
getDist g from to = let Just d = lookup to $ dist g from
                    in d

dist :: Graph -> Vertex -> [(Vertex,Int)] -- generates list of (vert,dist) pairs
dist g@(vs,es) s = d
                   where d = (s,0) :                    -- base
                           [ (v,
                              min' [ (find d u) + w     -- minimum edge
                                        | (u,w) <- adjwts v ])
                                 | v <- sortvs ] -- for each v
                         find dict x = let Just v = lookup x dict -- wrapper
                                       in v
                         min' xs = if xs == [] then -1 -- wrapper
                                   else minimum xs
                         sortvs = filter (/= s) $ toposort g
                         adjwts v = filter ((/= -1) . find d . fst)
                                     [ (fst $ fst e, snd e)
                                      | e <- (filter ((== v) . getD) es) ]
                                -- weights of adj. (u->v) vertices
--------------------------------------------

-- Shortest Path between Two Vertices --
getPath :: Graph -> Vertex -> Vertex -> (Int, [Vertex]) -- wrapper function
getPath g from to = let Just (w,p) = lookup to $ paths g from
                    in if (w,p) == (-1,[]) then (w,p)
                       else (w, reverse $ to : p)

paths :: Graph -> Vertex -> [(Vertex, (Int, [Vertex]))] -- generates list of (vert, (path weight, path)) pairs
paths g@(vs,es) s = p
                where p = (s, (0, [])) :                        -- base
                          [ (v, 
                             ( min' [ (w + (fst $ find p u),    -- minimum path
                                       u : (snd $ find p u))
                                      | (u,w) <- adjpts v ]))
                                      | v <- sortvs ]           -- for each v
                      find dict x = let Just (w,p) = lookup x dict -- wrapper
                                    in (w,p)
                      min' xs = if xs == [] then (-1,[])        -- wrapper
                                else head $ sortBy (comparing fst) xs
                      sortvs = filter (/= s) $ toposort g
                      adjpts v = filter ((/= -1) . fst . find p . fst)
                                   [ (fst $ fst e, snd e)
                                       | e <- (filter ((== v) . getD) es) ]
                             -- weights of paths to adj. (u->v) vertices
----------------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let g = read (head x) :: Graph
          let [i,j] = [read t :: Int | t <- take 2 $ tail x]
          -- print (getDist g i j)
          print (getPath g i j)
----------

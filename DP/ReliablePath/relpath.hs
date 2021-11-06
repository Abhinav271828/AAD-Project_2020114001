import System.Environment
import Data.List
import Data.Ord

type Vertex = Float
type Edge = (Int,Float,Float)
type Graph = ([Float],[(Int,Float,Float)])

-- Shortest Reliable Path Length --
srpl :: Graph -> Float -> Int -> Int -> Float
srpl g s d k = (lengths g s k) !! d !! k

lengths :: Graph -> Float -> Int -> [[Float]]
lengths g@(vs,es) s k = p
      where p = [ if (v == s) then (replicate (k+1) (0))
                  else (1/0) : 
                         [ min' 
                             [ l + (p !! u !! (i-1))
                                 | (u,_,l) <- filter ((== v) . getDest) es ]
                             | i <- [1..k] ]
                             | v <- vs     ]
            min' xs = if (xs == []) then (1/0) else minimum xs

getDest :: Edge -> Float
getDest (_,d,_) = d
-----------------------------------

-- Shortest Reliable Path --
srp :: Graph -> Float -> Int -> Int -> (Float,[Edge])
srp g s d k = (paths g s k) !! d !! k

paths :: Graph -> Float -> Int -> [[(Float,[Edge])]]
paths g@(vs,es) s k = p
        where p = [ if (v == s) then (replicate (k+1) (0,[]))
                    else ((1/0),[]) :
                            [ min'
                                [ (\(a,b) -> (a+l,(u,v,l):b)) 
                                  (p !! u !! (i-1)) 
                                      | (u,_,l) <- filter ((== v) . getDest) es ]
                                  | i <- [1..k] ] 
                                  | v <- vs ]
              min' xs = if (xs == []) then ((1/0),[])
                        else head $ sortBy (comparing fst) xs
----------------------------

-- Main --
main :: IO ()
main = do [g,s,d,k] <- getArgs
          let g' = read g :: Graph
          let s' = read s :: Float
          let d' = read d :: Int
          let k' = read k :: Int
          -- print (srpl g' s' d' k')
          print (srp g' s' d' k')
----------

import System.Environment
import Data.Ord
import Data.List

-- Longest Increasing Subsequence --
getLIS :: [Int] -> [Int] -- finds longest increasing subsequence of xs
getLIS xs = let l = zip [0..] xs
                maxs = [getPath xs x | x <- l] -- finds longest path from each element
            in snd $ last $ sortBy (comparing fst) maxs -- finds longest among them

getPath :: [Int] -> (Int,Int) -> (Int,[Int]) -- given list l and (i,x) s.t. l[i] = x, finds longest path starting from x
getPath l e = (w+1, reverse $ (l!!i):p) -- adds final element
                where (i,(w,p)) = last $ sortBy (comparing (fst . snd)) $ paths (zip [0..] l) e -- finds longest path

paths :: [(Int,Int)] -> (Int,Int) -> [(Int, (Int, [Int]))] -- generates list of (indx, (path length, path)) pairs
paths l (i,x) = p
                where p = (i, (0, [])) :                        -- base
                          [ (j, 
                             ( max' [ (1 + (fst $ find p s),    -- maximum length
                                       y : (snd $ find p s))
                                      | (s,y) <- adjpts (j,v) ])) -- get adjacent elements
                                      | (j,v) <- filter (/= (i,x)) l ]           -- for each v
                      find dict x = let Just (w,p) = lookup x dict -- wrapper
                                    in (w,p)
                      max' xs = if xs == [] then (-1,[])           -- wrapper
                                else last $ sortBy (comparing fst) xs
                      adjpts (j,v) = filter ((/= -1) . fst . find p . fst) $
                                   filter (((>) v) . snd) $ take j l
                             -- lengths of paths to adj. (u->v) vertices
----------------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let l = read (head x) :: [Int]
          print (getLIS l)
----------

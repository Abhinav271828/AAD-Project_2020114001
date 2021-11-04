import System.Environment
import Data.Ord
import Data.List

-- Maximum Value With Rep. --
value :: [(Int,Int)] -> Int -> Int
value wvs w = (sacks (sortBy (comparing fst) wvs)) !! w

sacks :: [(Int,Int)] -> [Int]
sacks wvs = g
        where g = (replicate minw 0) ++
                     [ maximum [ g !! (i-w) + v | (w,v) <- filter ((>=) i . fst) wvs]
                         | i <- [minw..] ]
              minw = fst $ head wvs
-----------------------------

-- Maximum-Value Combination With Rep. --
items :: [(Int,Int)] -> Int -> [(Int,Int)]
items wvs w = (combs (sortBy (comparing fst) wvs)) !! w

combs :: [(Int,Int)] -> [[(Int,Int)]]
combs wvs = g
       where g = (replicate minw []) ++
                    [ head $ sortBy (comparing (negate . sum . (map snd)))
                              [ (w,v) : (g !! (i-w)) | (w,v) <- filter ((>=) i . fst) wvs ]
                          | i <- [minw..] ]
             minw = fst $ head wvs
-----------------------------------------

-- Maximum-Value Combination (Eff) With Rep. --
items' :: [(Int,Int)] -> Int -> (Int,[(Int,Int)])
items' wvs w = (combs' (sortBy (comparing fst) wvs)) !! w

combs' :: [(Int,Int)] -> [(Int,[(Int,Int)])]
combs' wvs = g
       where g = (replicate minw (0,[])) ++
                    [ head $ sortBy (comparing (negate . fst))
                             [ (\(a,b) -> (a+v, (w,v):b)) (g !! (i-w))
                                | (w,v) <- filter ((>=) i . fst) wvs ]
                                | i <- [minw..] ]
             minw = fst $ head wvs
-----------------------------------------------

-- Maximum Value Without Rep. --
val :: [(Int,Int)] -> Int -> Int
val wvs w = last $ (sack $ sortBy (comparing fst) wvs) !! w

sack :: [(Int,Int)] -> [[Int]]
sack wvs = g
     where g = (replicate minw [0 | _ <- wvs]) ++
                  [ (snd $ head wvs) : 
                     [ if (w >= wj) then
                         max ((g !! (w-wj) !! (j-1)) + vj) (g !! w !! (j-1))
                       else (g !! w !! (j-1))
                         | (j,(wj,vj)) <- zip [1..] (tail wvs) ]
                         | w <- [minw..] ]
           minw = fst $ head wvs
--------------------------------

-- Maximum-Value Combination Without Rep. --
itm :: [(Int,Int)] -> Int -> (Int,[(Int,Int)])
itm wvs w = last $ cmb (sortBy (comparing fst) wvs) !! w

cmb :: [(Int,Int)] -> [[(Int,[(Int,Int)])]]
cmb wvs = g
    where g = (replicate minw [(0,[]) | _ <- wvs]) ++
                 [ (snd $ head wvs, [head wvs]) :
                      [ if (w >= wj) then
                          (maxByF ( (\(a,b) -> (a+vj, (wj,vj):b) ) (g !! (w-wj) !! (j-1)))
                                 (g !! w !! (j-1)))
                        else (g !! w !! (j-1))
                         | (j,(wj,vj)) <- zip [1..] (tail wvs) ]
                         | w <- [minw..] ]
          minw = fst $ head wvs
          maxByF a@(a1,a2) b@(b1,b2) = if (a1 >= b1) then a else b
--------------------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: [(Int,Int)]
          let j = read (head $ tail x) :: Int
          -- print (value i j)
          -- print (items i j)
          print (items' i j)
          -- print (val i j)
          -- print (itm i j)
----------

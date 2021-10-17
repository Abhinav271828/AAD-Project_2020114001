import System.Environment

type Matr a = [[a]]

-- Naive Method --
mult1 :: Num a => Matr a -> Matr a -> Matr a -- uses transpose and dot product
mult1 m1 m2 = [ [ dotp r c | c <- (transp m2)] | r <- m1]

transp :: Matr a -> Matr a
transp m@(r:rs) = case r of
                    [] -> []
                    _  -> (map head m):
                             (transp $ map tail m)

dotp :: Num a => [a] -> [a] -> a
dotp xs ys = sum $ zipWith (*) xs ys
------------------

-- Strassen's Algorithm --
mult2 :: Num a => Matr a -> Matr a -> Matr a
  -- splits even matrices; pads, multiplies and strips odd matrices
mult2 [a] b = [[dotp a (map head b)]]
mult2 m1 m2 = case (length m1 `mod` 2) of
                1 -> let p = mult2 ((0:[0 | _ <- (head m1)]) : (map (0:) m1))
                                   ((0:[0 | _ <- (head m2)]) : (map (0:) m2))
                     in tail (map tail p)
                0 -> join (p4 `plus` p5 `plus` p6 `minus` p2)
                          (p1 `plus` p2)
                          (p3 `plus` p4)
                          (p1 `plus` p5 `minus` p3 `minus` p7)
                           where ((a,b),(c,d)) = split m1
                                 ((e,f),(g,h)) = split m2
                                 p1 = a `mult2` (f `minus` h)
                                 p2 = (a `plus` b) `mult2` h
                                 p3 = (c `plus` d) `mult2` e
                                 p4 = d `mult2` (g `minus` e)
                                 p5 = (a `plus` d) `mult2` (e `plus` h)
                                 p6 = (b `minus` d) `mult2` (g `plus` h)
                                 p7 = (a `minus` c) `mult2` (e `plus` f)
                                 plus  = zipWith (zipWith (+))
                                 minus = zipWith (zipWith (-))

split :: Matr a -> ((Matr a, Matr a), (Matr a, Matr a))
split m = (unzip $ map half r1 , unzip $ map half r2)
             where (r1,r2) = half m
                   half l = splitAt ((length l) `div` 2) l

join :: Matr a -> Matr a -> Matr a -> Matr a -> Matr a
join a b c d = (zipWith (++) a b) ++ (zipWith (++) c d)
--------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let [i,j] = [read t :: [[Int]] | t <- take 2 x]
          -- print (mult1 i j)
          print (mult2 i j)
----------

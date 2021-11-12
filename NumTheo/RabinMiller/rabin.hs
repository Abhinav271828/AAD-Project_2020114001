import System.Random
import System.Environment

-- Rabin-Miller Primality Test --
isPrime :: Integer -> Integer -> Bool
isPrime k p
  | p `mod` 2 == 0 = if (p == 2) then True else False
  | otherwise = let a = getRnd (mkStdGen 2021) k (p-1) [1..p-1]
                    s = until ((== 1) . (`mod` 2))
                              (`div` 2) (p-1)
                    t = (p-1) `div` s
                    apows i = [ ((a !! i) ^ (s * j) `mod` p)
                                    | j <- unfold (== t) (* 2) 1 ]
                in and [ if ((((a !! (fromInteger i)) ^ (p-1)) `mod` p) /= 1)
                         then False
                         else ((all (== 1) pows) ||
                               ((last $ takeWhile (/= 1) pows) == (p-1)))
                                | i <- [0..(k-1)], pows <- [apows
                                                            (fromInteger i)] ]

getRnd :: StdGen -> Integer -> Integer -> [Integer] -> [Integer]
getRnd gen k l zp@(x:xs)
        | k == 0 = []
        | k == l = zp
        | otherwise = if rand
                      then (x : (getRnd gen' (k-1) (l-1) xs))
                      else (getRnd gen' k (l-1) xs)
                         where (r, gen') = (random gen :: (Int, StdGen))
                               rand = r `mod` 2 == 0

unfold :: (a -> Bool) -> (a -> a) -> a -> [a]
unfold p f x | p x = [x]
             | otherwise = x : (unfold p f (f x))
---------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let [i,j] = [ read t :: Integer | t <- (take 2 x) ]
          print (isPrime i j)
----------

import System.Environment

-- Euclid's GCD Algorithm --
-- gcd             :: (Integral a) => a -> a -> a
-- {-# NOINLINE [1] gcd #-}
-- gcd x y         =  gcd' (abs x) (abs y)
--                    where gcd' a 0  =  a
--                          gcd' a b  =  gcd' b (a `rem` b)
----------------------------

-- Euclid's Extended GCD Algorithm --
extEuc :: Integer -> Integer -> (Integer, Integer, Integer)
extEuc a 0 = (1,0,a)
extEuc a b = let (x,y,d) = extEuc b (a `rem` b)
             in (y, x - (a `div` b) * y, d)
----------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let [i,j] = [read n :: Integer | n <- take 2 x]
          -- print (gcd i j)
          print (extEuc i j)
----------

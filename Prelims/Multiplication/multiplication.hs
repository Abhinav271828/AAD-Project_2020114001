import System.Environment
import Data.Char

-- Part 1 : Lists of Integers --
-- Haskell Default --
mult1 :: [Int] -> [Int] -> [Int] -- Haskell's native multiplication
mult1 x y = lst $ (dec x) * (dec y)
                where dec l = read (l >>= show) :: Integer
                      lst n = map (\t -> (ord t) - 48) (show n)
---------------------

-- School Algorithm --
mult2 :: [Int] -> [Int] -> [Int] -- treats input as lists of digits
mult2 x y = toNum [] $ reverse $ foldr (\b p -> zipWith (+) b (0:p)) (repeat 0) t
                where t = [ (map (*d) x) ++ z | d <- y]
                      z = tail [0 | _ <- y]

toNum :: [Int] -> [Int] -> [Int] -- performs carry over
toNum l r@(x:xs) = case xs of
                     (_:_) -> toNum ((x `mod` 10) : l)
                                    (((head xs) + (x `div` 10)) : (tail xs))
                     []    -> x:l
----------------------

-- Karatsuba Algorithm --
mult3 :: [Int] -> [Int] -> [Int] -- inputs are treated as lists of digits
mult3 [a]  b  = toNum [] $ map (*a) (reverse b)
mult3  a  [b] = toNum [] $ map (*b) (reverse a)
mult3  a   b  = toNum [] $ reverse $ foldr (plus) [0]
                                 [ab1 ++ (replicate (2*d) 0) ,
                                  ab  ++ (replicate d     0) ,
                                  ab2]
                   where ab1 = (a1 `mult3` b1)
                         ab2 = (a2 `mult3` b2)
                         ab  = ((a1 `plus` a2) `mult3` (b1 `plus` b2))
                                        `minus` (ab1 `plus` ab2)
                         ((a1,a2),(b1,b2),d) = split a b
                         plus x y  = zipWith (+)
                                      x
                                      ((replicate ((length x) -
                                                      (length y)) 0) ++ y)
                         minus x y = zipWith (-)
                                      x
                                      ((replicate ((length x) -
                                                      (length y)) 0) ++ y)

split :: [Int] -> [Int] -> (([Int],[Int]),([Int],[Int]),Int)
  -- splits strings, typecasts, and returns length of second part
split x y = let l1  = length x
                l2  = length y
                d   = min (l1 `div` 2) (l2 `div` 2)
                d1  = l1 - d
                d2  = l2 - d
            in ( ( take d1 x, drop d1 x) ,
                 ( take d2 y, drop d2 y) , d)

-------------------------

-- Part 2 : Normal Representation --
-- Haskell Default --
mult4 :: Integer -> Integer -> Integer -- Haskell's native multiplication
mult4 = (*)
---------------------

-- School Algorithm --
mult5 :: Integer -> Integer -> Integer -- treats input as lists of digits
mult5 a b = dec $ toNum [] $
                    reverse $ foldr (\b p -> zipWith (+) b (0:p)) (repeat 0) t
                      where t = [ (map (*d) x) ++ z | d <- y]
                            z = tail [0 | _ <- y]
                            x = map (\t -> (ord t) - 48) (show a)
                            y = map (\t -> (ord t) - 48) (show b)
                            dec l = read (l >>= show) :: Integer
----------------------

-- Karatsuba Algorithm --
mult6 :: Integer -> Integer -> Integer -- input is two integers
mult6 a b
  | (a < 10) || (b < 10)
              = a * b
  | otherwise = ((10^(2*d) * (ab1)) +
                 (10^(d)   * (ab)) + 
                             (ab2))
                    where ab1 = a1 `mult6` b1
                          ab2 = a2 `mult6` b2
                          ab  = (a1+a2) `mult6` (b1+b2) - ab1 - ab2
                          ((a1,a2),(b1,b2),d) = split2 (show a) (show b)

split2 :: String -> String -> ((Integer,Integer),(Integer,Integer),Int)
  -- splits strings, typecasts, and returns length of second part
split2 x y = let l1  = length x
                 l2  = length y
                 d   = min (l1 `div` 2) (l2 `div` 2)
                 d1  = l1 - d
                 d2  = l2 - d
                 int = \s -> read s :: Integer
             in ( ( int $ take d1 x, int $ drop d1 x) ,
                  ( int $ take d2 y, int $ drop d2 y) , d)

-------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let [i,j] = [read t :: [Int] | t <- take 2 x]
          -- print (mult1 i j)
          -- print (mult2 i j)
          -- print (mult3 i j)
          let [i,j] = [read t :: Integer | t <- take 2 x]
          print (mult4 i j)
          -- print (mult5 i j)
          -- print (mult6 i j)
----------

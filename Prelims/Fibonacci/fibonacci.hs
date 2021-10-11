import System.Environment

-- Naive Solution --
fib1 :: Int -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = (fib1 (n-1)) + (fib1 (n-2))
--------------------

-- Memoised Solution --
fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

fib2 :: Int -> Integer
fib2 = (!!) fibs
-----------------------

-- Matrix Solution -- (naive multiplication)
type Matr a = [[a]] -- datatype of matrices

mult :: Num a => Matr a -> [a] -> [a] -- multiply 2x2 with 2x1 matrix
mult [[a,b],[c,d]] [e,f]
    = [ a * e + b * f , 
        c * e + d * f ]

appl :: Int -> (a -> a) -> a -> a -- apply f to x, n times
appl n f x = foldr ($) x (replicate n f)

fib3 :: Int -> Integer
fib3 n = head $ appl n (mult r) b
             where r = [[0,1],[1,1]] :: Matr Integer
                   b = [0,1] :: [Integer]
---------------------

-- Logarithmic multiplication --
pow :: Num a => Int -> Matr a -> Matr a -- logarithmic multiplication
pow 0 m = [[1,0],[0,1]]
pow 1 m = m
pow n m = case (n `mod` 2) of
           0 -> (sqr $ pow (n `div` 2) m)
           1 -> (sqr $ pow (n `div` 2) m) `mul` m
                where mul [[a,b],[c,d]] [[e,f],[g,h]]
                          = [[a*e + b*g, a*f + b*h],
                             [c*e + d*g, c*f + d*h]]

sqr :: Num a => Matr a -> Matr a -- squaring a 2x2 matrix
sqr [[a,b],[c,d]] = [ [ a^2 + bc , ad * b ] ,
                      [ ad * c , bc + d^2 ] ]
                          where bc = b * c
                                ad = a + d

fib4 :: Int -> Integer
fib4 n = head $ mult (pow n r) b
             where r = [[0,1],[1,1]] :: Matr Integer
                   b = [0,1] :: [Integer]
--------------------------------

-- Strassen with Logarithm --
pow2 :: Num a => Int -> Matr a -> Matr a -- binary exponentiation with Strassen
pow2 0 _ = [[1,0],[0,1]]
pow2 1 m = m
pow2 n m = case (n `mod` 2) of
            0 -> (sqr $ pow (n `div` 2) m)
            1 -> (sqr $ pow (n `div` 2) m) `mul` m
                 where mul [[a,b],[c,d]] [[e,f],[g,h]]
                           = [[m1+m4+m7-m5, m3+m5],
                              [m2+m4, m1+m3+m6-m2]]
                            where m1 = (a+d) * (e+h)
                                  m2 = (c+d) * e
                                  m3 = a * (f-h)
                                  m4 = d * (g-e)
                                  m5 = (a+b) * h
                                  m6 = (c-a) * (e+f)
                                  m7 = (b-d) * (g+h)

fib5 :: Int -> Integer
fib5 n = head $ mult (pow2 n r) b
             where r = [[0,1],[1,1]] :: Matr Integer
                   b = [0,1] :: [Integer]
-----------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: Int
          -- print (fib1 i)
          -- print (fib2 i)
          -- print (fib3 i)
          -- print (fib4 i)
          print (fib5 i)
----------

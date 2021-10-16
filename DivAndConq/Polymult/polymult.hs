import System.Environment
import Data.Complex

-- Naive Method --
pmul1 :: Num a => [a] -> [a] -> [a]
  -- inputs are lists of coefficients starting from 0
pmul1 a b = [ sum [ x*y | i <- [0..d], j <- [0..d],
                         i+j==k, i <= d1, j <= d2,
                         x <- [a !! i], y <- [b !! j] ] | k <- [0..d] ]
                where d1 = (length a) - 1
                      d2 = (length b) - 1
                      d = d1 + d2
------------------

-- FFT Algorithm --
pmul2 :: [Double] -> [Double] -> [Double]
pmul2 a b = map (/ n) $ map realPart $
               fft (zipWith (*) vala valb) (1 / wn) n
             where d = fromIntegral ((length a) + (length b) - 2) :: Double
                   n = until (> d) (*2.0) 2.0 
                   vala = fft (map comp a) wn n
                   valb = fft (map comp b) wn n
                   wn = cis (2 * pi / n) :: Complex Double

comp :: Double -> Complex Double
comp a = a :+ 0.0

fft :: [Complex Double] -> Complex Double -> Double -> [Complex Double]
fft a (1.0 :+ _) _ = [sum a]
fft a w        n = (zipWith3 (\e p o -> e + p*o)
                        evens pows odds) ++
                   (zipWith3 (\e p o -> e - p*o)
                        evens pows odds)
                       where evens = (fft even (w ^ 2) h)
                             odds  = (fft odd  (w ^ 2) h)
                             pows  = [w^i | i <- [0..(round h)]]
                             h     = n / 2
                             (even,odd) = split a

split :: [a] -> ([a],[a])
split []  = ([],[])
split [x] = ([x],[])
split (a:as) = (\(e,o) -> (a:o,e)) (split as)
-------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let [i,j] = [read t :: [Int] | t <- take 2 x]
          print (pmul1 i j)
          -- let [i,j] = [read t :: [Double] | t <- take 2 x]
          -- print (pmul2 i j)

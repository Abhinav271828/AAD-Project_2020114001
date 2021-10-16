import System.Environment

merge :: Ord a => [a] -> [a] -> [a]
  -- merges two lists; common to both functions
merge x        []       = x
merge []       y        = y
merge a@(x:xs) b@(y:ys) = if x < y
                             then x:(merge xs b)
                             else y:(merge a ys)

-- Recursive Mergesort --
mergesort1 :: Ord a => [a] -> [a] -- ordinary implementation
mergesort1 []  = []
mergesort1 [x] = [x]
mergesort1 xs  = merge (mergesort1 left) (mergesort1 right)
                   where (left,right) = splitAt ((length xs)
                                                  `div` 2) xs
-------------------------

-- Iterative Mergesort --
mergesort2 :: Ord a => [a] -> [a] -- uses a queue
mergesort2 xs = head $ until ((== []) . tail)
                         (\(q:r:qs) -> qs ++ [merge q r])
                         [[x] | x <- xs]

-- from the Standard Prelude
-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
-- until                   :: (a -> Bool) -> (a -> a) -> a -> a
-- until p f = go
--   where
--     go x | p x          = x
--          | otherwise    = go (f x)
-------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let l = read (head x) :: [Int]
          print (mergesort1 l)
          -- print (mergesort2 l)
----------

import System.Environment

-- Activity Selection --
data Activity = A Int Int -- (start time, finish time) pairs
                    deriving (Show,Eq,Read)

instance Ord Activity where -- ordering according to finish time
  (A _ f1) < (A _ f2) = f1 < f2
  compare (A _ f1) (A _ f2) = compare f1 f2

select :: [Activity] -> [Activity] -- takes list and returns compatible activities
select = (select' []) . mergesort

select' :: [Activity] -> [Activity] -> [Activity] -- first argument has previously selected activities
select' []             (x         :xs) = select' [x] xs -- first activity
select' ss             []              = ss             -- all done
select' ss@((A _ f):_) (x@(A s' _):xs) = if (s' >= f)   -- if compatible
                                           then select' (x:ss) xs -- add
                                           else select' ss xs -- else leave

merge :: Ord a => [a] -> [a] -> [a]
merge x        []       = x
merge []       y        = y
merge a@(x:xs) b@(y:ys) = if x < y
                             then x:(merge xs b)
                             else y:(merge a ys)

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort left) (mergesort right)
                   where (left,right) = splitAt ((length xs)
                                                  `div` 2) xs
------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let i = read (head x) :: [Activity]
          print (select i)
----------

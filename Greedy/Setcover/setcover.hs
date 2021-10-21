import System.Environment
import Data.IntSet as S hiding (map, filter)
import Data.List
import Data.Ord

-- Set Cover Problem --
setcover :: [IntSet] -> IntSet-> [IntSet]
setcover si b = if (b == fromList [])
                then []
                else set : (setcover left (S.difference b set))
                     where byUncv = sortBy (comparing
                                              (negate . uncovered))
                                           si
                           set = head byUncv
                           left = tail byUncv
                           uncovered s = size (S.intersection s b)
-----------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          -- let i = map fromList (read (head x) :: [[Int]])
          -- let j = fromList (read (head (tail x)) :: [Int])
          -- print (map toList $ setcover i j)
          let i = read (head x) :: [IntSet]
          let j = read (head (tail x)) :: IntSet
          print (setcover i j)
----------

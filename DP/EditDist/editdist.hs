import System.Environment

-- Edit Distance --
diff :: Char -> Char -> Int -- difference between two chars
diff a b = if (a == b) then 0 else 1

min3 :: Int -> Int -> Int -> Int -- minimum of 3 numbers
min3 m n p = minimum [m,n,p]

editdist :: String -> String -> Int -- returns edit distance
editdist x y = (last . last) $ dists x y

dists :: String -> String -> [[Int]] -- table of prefix distances
dists x y = t
  where t = [0..n] :                                    -- first row
                  [ i :                                 -- first column
                        [ min3 (1 + (t !! (i-1) !! j))  -- cell above
                               (1 + (t !! i !! (j-1)))  -- cell to left
                               (diff (x !! (i-1)) (y !! (j-1)) +
                                  (t !! (i-1) !! (j-1))) -- cell to top left
                                    | j <- [1..n] ]
                                    | i <- [1..m] ]
        m = length x
        n = length y
-------------------

-- Edit Alignment --
edit :: String -> String -> (Int,(String,String)) -- edit distance with alignment
edit x y = (last . last) $ edits x y

edits :: String -> String -> [[(Int,(String,String))]] -- table with alignments
edits x y = t
  where t = [ (j, (replicate j ' ', take j y)) | j <- [0..n]] : -- first row
              [ (i,(take i x, replicate i ' ')) :               -- first column
                   [ extend i j | j <- [1..n] ]
                                | i <- [1..m] ]
        m = length x
        n = length y
        extend i j = let (etop,(xtop,ytop)) = (t !! (i-1) !! j) -- cell above
                         (eleft,(xleft,yleft)) = (t !! i !! (j-1)) -- cell to left
                         (etopleft,(xtopleft,ytopleft)) = (t !! (i-1) !! (j-1)) -- cell to top left
                         d = diff (x !! (i-1)) (y !! (j-1)) -- diff value
                     in if (1 + etop <= 1 + eleft && -- comparison
                            1 + etop <= d + etopleft)
                        then (1 + etop,
                                (xtop ++ [x !! (i-1)], ytop ++ " "))
                        else if (1 + eleft <= d + etopleft)
                             then (1 + eleft,
                                     (xleft ++ " ", yleft ++ [y !! (j-1)]))
                             else (diff (x !! (i-1)) (y !! (j-1))
                                   + etopleft,
                                      (xtopleft ++ [x !! (i-1)], 
                                        ytopleft ++ [y !! (j-1)]))
--------------------

-- Efficient Edit Alignment --
editE :: String -> String -> (Int,(String,String)) -- edit distance with alignment
editE x y = (\(n,(a,b)) -> (n, (reverse a, reverse b))) $
                (last . last) $ editsE x y

editsE :: String -> String -> [[(Int,(String,String))]] -- table with alignments
editsE x y = t
  where t = [ (j, (replicate j ' ', reverse $ take j y)) | j <- [0..n]] : -- first row
              [ (i,(reverse $ take i x, replicate i ' ')) :               -- first column
                   [ extend i j | j <- [1..n] ]
                                | i <- [1..m] ]
        m = length x
        n = length y
        extend i j = let (etop,(xtop,ytop)) = (t !! (i-1) !! j) -- cell above
                         (eleft,(xleft,yleft)) = (t !! i !! (j-1)) -- cell to left
                         (etopleft,(xtopleft,ytopleft)) = (t !! (i-1) !! (j-1)) -- cell to top left
                         d = diff (x !! (i-1)) (y !! (j-1)) -- diff value
                     in if (1 + etop <= 1 + eleft && -- comparison
                            1 + etop <= d + etopleft)
                        then (1 + etop,
                                ((x !! (i-1)) : xtop, ' ' : ytop))
                        else if (1 + eleft <= d + etopleft)
                             then (1 + eleft,
                                     (' ' : xleft, (y !! (j-1)) : yleft))
                             else (diff (x !! (i-1)) (y !! (j-1))
                                   + etopleft,
                                      ((x !! (i-1)) : xtopleft, 
                                        (y !! (j-1)) : ytopleft))
------------------------------

-- Main --
main :: IO ()
main = do x <- getArgs
          let [i,j] = take 2 x
          -- print (editdist i j)
          -- print (edit i j)
          print (editE i j)
----------

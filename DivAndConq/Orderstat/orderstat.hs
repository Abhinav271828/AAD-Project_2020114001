import System.Environment

-- Deterministic Approach --
selection :: Ord a => [a] -> Int -> a -- finds the kth smallest elmt
selection [x] _ = x
selection ls k = if (k < (length smaller))
                    then selection smaller k
                 else if (k < (length equal) + (length smaller))
                    then piv
                 else selection larger (k - (length equal) - (length smaller))
                        where piv = median $ map med5 $ div5 ls
                              smaller = filter (< piv)  ls
                              equal   = filter (== piv) ls
                              larger  = filter (> piv)  ls

div5 :: [a] -> [[a]] -- divides list into lists of 5 elmts each
div5 [] = []
div5 ls = if (length ls <= 5) then [ls]
            else (take 5 ls):(div5 $ drop 5 ls)

med5 :: Ord a => [a] -> a -- finds median of a list of ≤ 5 elmts
med5 [x] = x
med5 ls = (sort [] ls) !! ((length ls) `div` 2)
             where sort left [] = left
                   sort left (x:xs) = sort (insert x left) xs
                   insert x l = (takeWhile (< x) l)
                                 ++ x:(dropWhile (< x ) l)

median :: Ord a => [a] -> a -- median of arbitrary list
median xs = selection xs ((length xs) `div` 2)
----------------------------

-- Main --
main = do x <- getArgs
          let i = read (head x) :: [Int]
          print (median i)
----------

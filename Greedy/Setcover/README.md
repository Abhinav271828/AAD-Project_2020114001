# The Set Cover Problem
A single greedy algorithm was taught in class to solve the set cover problem (which is NP-complete). It does not always provide an optimum solution.  

The code uses the `Data.IntSet` library, which provides methods to define and manipulate sets of integers.

## Running
Note that the `main` function has been divided into two parts. The first part expects sets as inputs; hence the input must be in list format preceded with the keyword `fromList` (enclosed in quotes). The first argument is the list of sets from which the cover is to be picked; the second argument is the set to be covered. The output is also in the set format.
```
> ./setcover "[fromList [1,18,9,4], fromList [4,1,19,8], fromList [4,18,1,9,14], fromList [8,5,1,18,4], fromList [12,15,19,20], fromList [19,8,21,13], fromList [19,15,21,5], fromList [19,12,1,20,5], fromList [19,13,1,18,5], fromList [20,8,18,5,1,4], fromList [12,9,4], fromList [18,15,1,19,20]]" "fromList [1,4,5,8,9,12,14,15,18,19,20,21]"
> [fromList [1,4,5,8,18,20],fromList [12,15,19,20],fromList [1,4,9,14,18],fromList [5,15,19,21]]
```
The second part expects lists as input and prints lists as output, and typecasts them to and from sets internally. Thus it takes longer but its I/O is more readable.
```
> ./setcover "[[1,18,9,4], [4,1,19,8], [4,18,1,9,14], [8,5,1,18,4], [12,15,19,20], [19,8,21,13], [19,15,21,5], [19,12,1,20,5], [19,13,1,18,5], [20,8,18,5,1,4], [12,9,4], [18,15,1,19,20]]"
> "[1,4,5,8,9,12,14,15,18,19,20,21]"
```

To run one of the parts, uncomment it and pass the arguments as shown.

## Explanation
As mentioned above, the code uses the `Data.IntSet` library to manipulate and define sets. Given a list of sets `si` (from which a cover is to be picked) and a set `b` (which is to be covered), the function `uncovered` determines how many elements in `b` would be covered by any given set.  
The sets in `si` are then ordered according to their `uncovered` value (in descending order, hence the `negate`), and the first set (named `set`) is taken as the next set in the cover. This set is therefore appended to the result of the code on the remaining sets in `si` (which is `left`) and the remaining part of `b` (found by taking the set difference between `b` and `set`).  

This implementation has not been benchmarked due to the number of variables which are necessarily to fix an instance of the problem – the number of sets, the size of the sets, the contents of the sets – and the constraints on these variables. In any case, the algorithm is not intended to be efficient (since the problem itself is NP-complete).

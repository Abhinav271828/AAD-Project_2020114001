# Longest Increasing Sequence
One algorithm was taught in class to find the longest increasing sequence – to treat the list as a DAG and find the longest path in it.
 
## Running
To run the code, pass the list of numbers (enclosed in quotes) as a command-line argument.
```
./LIS "[2,7,1,8,2,8]"
[1,2,8]
```

## Explanation
There is a wrapper function `getLIS`, which finds the longest increasing subsequence from each element and picks the maximum-length of all subsequences. The subroutine `getPath` uses the `paths` function to find the LIS from a certain element. 

`paths` proceeds in a manner similar to the function of the same name in the "Shortest Path in a DAG" code. However, it accepts a list of pairs, each of which has its index as its first element and the value at that index as the second element. This is to avoid repeatedly accessing the list (which takes linear time).  

When `(i,x)` is passed, the path up to itself is marked as empty. For all other elements `(j,v)`, the `adjpts` finds the elements before it in the list and having value less than it; then one is added to the path length of each such element and the element itself is appended to the path. The maximum of all such path lengths is found and added to the list.

## Analysis
The R² values of the best fit of various types of functions for the algorithm's running times are shown below.  

Linear | Quadratic | Cubic | Biquadratic | Exponential | Power Series | Logarithmic  
------ | --------- | ----- | ----------- | ----------- | ------------ | -----------  
0.799  | 0.986     | 0.996 | 0.996       | 0.985       | 0.723        | 0.403  

We note first that the biquadratic and cubic curves have a very high R² value, although the biquadratic equation's leading coefficient is extremely small. A theoretical analysis of the code shows that the time is bounded by O(n^4).  

First, `getLIS` runs `getPath` *n* times and traverses the resulting list; therefore the total time is *n*T(`getPath`) + *n*.  

Secondly, `getPath` runs `paths`, and then traverses the list; this takes *n*T(`paths`) + *n*. The final list has length ≤ *n*; therefore the time for `reverse` is also bounded by *n*.  

Now we consider `paths`. For each element (a factor of *n*), it runs `adjpts`, which filters *l* and therefore takes time linear in *n*. It also runs `max'` and `find` on the elements returned by `adjpts`; `max` will therefore take time < *n* (since at most *n*-1 points can be adjacent to the current point), and `find` will take time proportional to *n*.  

This gives us an overall time of *n*(*n*(*n*²) + *n*) + *n* = O(n^4). Therefore, the expectation of a quadratic value is justified.  

![Running Time of `getLIS`](LIS.png)

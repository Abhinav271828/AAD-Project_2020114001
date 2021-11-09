# AAD-Project_2020114001
Project for the Algorithm Analysis and Design course at IIIT Hyderabad (Haskell implementation of all algorithms)

# Tentative Timeline
The following is a tentative timeline of the work involved (covering all algorithms to date, *i.e.*, before 8th October).  

- [x] October 15th: up to Lecture 5
    - [x] Preliminary algorithms
        - [x] Fibonacci
        - [x] integer multiplication)
    - [x] All divide and conquer algorithms
        - [x] mergesort
        - [x] matrix multiplication
        - [x] order statistics
        - [x] polynomial multiplication
- [x] October 22nd: up to Lecture 8, 15th September (all greedy algorithms)
    - [x] Kruskal's algorithm
    - [x] Activity selection
    - [x] Huffman encoding
    - [x] The set cover problem
- [ ] November 10th: up to Lecture 12, 29th September (all dynamic programming algorithms)
    - [x] Shortest path in a DAG
    - [x] Longest increasing subsequence
    - [x] Edit distance
    - [x] Chain matrix multiplication
    - [x] The knapsack problem (with and without repetition)
    - [x] Shortest reliable path
    - [x] All pairs shortest path
    - [x] Maximal independent set
- [ ] November 15th: up to Lecture 14, 13th October (all number theoretic algorithms)
    - [ ] Euclid's GCD algorithm
    - [ ] Euclid's extended algorithm
    - [ ] The Rabin-Miller primality test

# Functional Programming
## The Lambda Calculus
Functional programming is a paradigm of programming developed from a theoretical computation model called the lambda calculus, created by Alonzo Church. Popular functional programming languages include Haskell, Lisp and Rust.  
The lambda calculus precedes the Turing machine model of computation (in fact, Church was Alan Turing's doctoral advisor). Although they are extremely divergent models in terms of their abstractness and the methods they require, they were later proven to be equivalent, substantiating what is now known as the Church-Turing Thesis.  

The most basic form of the lambda calculus is built around functions, with no notion of data or datatypes. All information – including numbers, booleans, etc. – is implemented as nameless functions (the imperative programmer will recognise that this forms the basis of Python's lambdas).

## Features of FP Languages
Possibly the most important feature of functional programming is *referential transparency*. Once a variable is declared to have a value, it can always be substituted for that value with no change to the validity of the program. To quote Miran Lipovača, author of [Learn You a Haskell](http://learnyouahaskell.com):  

> If you say that `a` is 5, you can't say it's something else later because you just said it was 5. What are you, some kind of liar?

An important result of referential transparency is that the correctness of programs can be proved rigorously by substituting function definitions in their calls until the result is obtained.  

This feature is closely related to the lack of *state* in functional programming, so prominent in imperative programs. Other consequences of stateless implementations are

* no iteration (as the counter continuously changes value)
* no side-effects (no memory manipulation or, notably, I/O)
* no global "accumulator" variables

All three of these features are extensively made use of in the pseudo-code shown in class, which means that a functional implementation of the same algorithms will require a considerably different thought-process to code.

## Haskell
Haskell is a general-purpose functional programming language. It has all the features of ordinary functional programming, with some additional ones.  

Firstly, Haskell is *strongly typed*, unlike some functional and some imperative languages (Common LISP and Python). Even in the absence of type declarations, types are inferred using a modified version of the Hindley-Milner algorithm.  

Secondly, Haskell is *lazy* – roughly, expressions are not evaluated until they absolutely have to be. An important consequence of this is that Haskell has the capacity for infinite lists, as long as one does not attempt to print them.  

Although Haskell is pure for the most part, it is possible to simulate state and side-effects (like I/O) using a feature called *monads*. Monads enable us to separate the impure parts of the code (involving side-effects, etc.) from the pure computation. This is not, however, frequently used outside such contexts.  
As a side note, monads also enable non-determinism to be simulated and error handling to be handled conveniently.

## An Example
One way to find the Fibonacci numbers (by generating a list, in linear time) is as follows:
```hs
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))
```

Note that function application is indicated by simply listing the arguments after the function, *e.g.* if `succ` adds 1 to its argument, then `(succ 5)` evaluates to 6.  

The `(:)` operator (sometimes called *cons*) attaches an element to the front of a list. It is right associative; thus the first two elements of `fibs` are 0 and 1.  

The `zipWith` function applies a binary operator (in this case `(+)`) to each pair of corresponding elements of two lists. Here, the two lists are `fibs` itself, and `(tail fibs)`, which is `fibs` without its first element.  

Since Haskell is lazy, when (say) the third element is requested, it tries to evaluate (the first element of `fibs`) `+` (the first element of `(tail fibs)`). Both of these are available and it returns 1, as needed.  
In this manner, any element of `fibs` (which is an infinite list) can be computed.

# Analysis of Algorithms
The algorithms have been timed using the bash script `bench.sh`. Each input is run a number of times to avoid noise as far as is practical.  

All the running time-related data obtained by using this script can be found [here](https://docs.google.com/spreadsheets/d/1Bw8u3r5KfDKLQmdH42RP8ePQ1ZesS4npszL40DaJ9t4/edit#gid=0). Graphs and best-fit lines (trendlines) were generated using Google Sheets.  

Note that the script uses the `gdate` command (which may be system-specific; `date` will run on a Linux machine) to find the time in microseconds. The name of the executable file that is to be timed is passed as a command line argument, *e.g.*, `./bench.sh "Prelims/Fibonacci/fibonacci"`.

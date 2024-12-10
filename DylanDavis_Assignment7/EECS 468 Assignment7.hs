{-
Dylan Davis
3047302
EECS 468
11/13/2023
haskell functions 
Collaborators: Chat GPT, Ashley Aldave 
-}

{-- 1
Replicate' function:
In a similar manner to the function length described in the “Haskell List 
Comprehension” lecture, show how the library function: 

replicate :: Int -> a -> [a], 

which produces a list of identical elements, can be defined
using list comprehension. (Name the new function replicate', so Haskell
will not give you an error). For example:
> replicate' 3 True
[True, True, True]
Show a screen print with your code using the following test case:
> replicate' 5 “test code”
--}
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]] -- n is the number of times you want it to replicate, and x is what is to be replicated 

{-- 2
perfects function:
    A positive integer is perfect if it equals the sum of all of its factors,
    excluding the number itself. Using a list comprehension, define a
    function:

    perfects :: Int -> [Int] 

    that returns the list of all perfect numbers up to a given limit. For example:

> perfects 500
[6,28,496]

Show a screen print with your code of:
> perfects 9000
--}

perfects :: Int -> [Int]
perfects limit = [n | n <- [1..limit], n == sum [x | x <- [1..n-1], n `mod` x == 0]] -- list comprehension, that will find all of the perfect numbers up to a given limit 

{-- 3
find function:
    Suppose that we represent a lookup table by a list of pairs of keys and
    values. Then for any type of keys that supports equality, define a function
    as follows: 
    
    find :: Eq a => a -> [(a,b)] -> [b] that returns the list of all

values that are associated with a given key in a table. For example:

> find 'b' [('a',1),('b',2),('c',3),('b',4)]
[2,4]

Show a screen print with your code of:
> find 'c' [('a',1),('b',2),('c',3),('b',4),('c',25)]
--}

find :: Eq a => a -> [(a, b)] -> [b] -- define the find is a function that takes in two arguments 
find key table = [value | (k, value) <- table, k == key] -- list comprehension that takes in key and table and will return the value at the desired key.

{-- 4
positions function:

Redefine the positions function from the “Haskell List Comprehension”
lecture using the find function. For example:
    > positions 0 [1,0,0,1,0,1,1,0]
    [1,2,4,7]
Show a screen print with your code using the following test case:
    > positions 1 [1,0,0,1,0,1,1,0]
--}
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..]) -- list comprehension that takes in an element that we want to find the position of (x) and the list we want to find it in (xs), here the zip function is a build in function of haskell that matches elements to their index

{-- 5
scalarproduct function:
The scalar product of two lists of integers xs and ys of length n is given by
the sum of the products of the corresponding integers:
For example:

    > scalarproduct [1,2,3] [4,5,6]
    32

Using a list comprehension and the zip function, define a function that
returns the scalar product of two lists.
Show a screen print with your code using the following test case:

    > scalarproduct [-1,2,3] [-4,-5,6]

--}

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys] -- list comprehension that takes in input lists used to calculate the scalar product, zip function pairs xs and ys pairs together, and the multiples x and y together, and finally sum, calculates the sum of all these products. 
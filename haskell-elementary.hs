-- Type the factorial function into a Haskell source file and load it into GHCi.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- Try examples like factorial 5 and factorial 1000.[3]
--     What about factorial (-1)? Why does this happen?
-- It falls through factorial 0 and lands in factorial n and never terminates

-- The double factorial of a number n is the product of every other number from 1 (or 2) up to n. For example, the double factorial of 8 is 8 × 6 × 4 × 2 = 384, and the double factorial of 7 is 7 × 5 × 3 × 1 = 105. Define a doublefactorial function in Haskell.
doubleFactorial :: Integer -> Integer
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

-- Expand out the multiplication 5 × 4 similarly to the expansion we used above for factorial 3.
-- mult 5 4 = mult 5 3 + 5 = mult 5 2 + 5 + 5 = mult 5 1 + 5 + 5 + 5 = mult 5 0 + 5 + 5 + 5 + 5 = 0 + 20 = 20

-- Define a recursive function power such that power x y raises x to the y power.
myPower :: Int -> Int -> Int
myPower x 0 = 1
myPower x y = x * myPower x (y - 1)
-- You are given a function plusOne x = x + 1. Without using any other (+)s, define a recursive function addition such that addition x y adds x and y together.
-- (Harder) Implement the function log2, which computes the integer log (base 2) of its argument. That is, log2 computes the exponent of the largest power of 2 which is less than or equal to its argument. For example, log2 16 = 4, log2 11 = 3, and log2 1 = 0. (Small hint: read the last phrase of the paragraph immediately preceding these exercises.)
myLog2 :: Double -> Double
myLog2 1 = 0
myLog2 x = myLog2 (x/2) + 1 


-- Exercises

-- Give recursive definitions for the following list-based functions. In each case, think what the base case would be, then think what the general case would look like, in terms of everything smaller than it. (Note that all of these functions are available in Prelude, so you will want to give them different names when testing your definitions in GHCi.)

-- replicate :: Int -> a -> [a], which takes a count and an element and returns the list which is that element repeated that many times. E.g. replicate 3 'a' = "aaa". (Hint: think about what replicate of anything with a count of 0 should be; a count of 0 is your 'base case'.)
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- (!!) :: [a] -> Int -> a, which returns the element at the given 'index'. The first element is at index 0, the second at index 1, and so on. Note that with this function, you're recursing both numerically and down a list[5].
(!!~) :: [a] -> Int -> a
(x:xs) !!~ 0 = x 
(x:xs) !!~ n = xs !!~ (n-1)

-- (A bit harder.) zip :: [a] -> [b] -> [(a, b)], which takes two lists and 'zips' them together, so that the first pair in the resulting list is the first two elements of the two lists, and so on. E.g. zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. If either of the lists is shorter than the other, you can stop once either list runs out. E.g. zip [1,2] "abc" = [(1, 'a'), (2, 'b')].
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys


-- Define myLength using an auxiliary function and an accumulating parameter, as in the loop-like alternate version of factorial.
myLength :: [a] -> Int
myLength lst = helper lst 0
    where 
        helper [] acc = acc
        helper (x:xs) acc = helper xs (acc + 1)

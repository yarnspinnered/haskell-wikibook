module Main where

import Data.List

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

-- takeInt returns the first n items in a list. So, takeInt 4 [11,21,31,41,51,61] returns [11,21,31,41].
takeInt :: Int -> [Int] -> [Int]
takeInt 0 _ = []
takeInt n (x:xs) = x : takeInt (n - 1) xs

-- dropInt drops the first n items in a list and returns the rest. So, dropInt 3 [11,21,31,41,51] returns [41,51].
dropInt :: Int -> [Int] -> [Int]
dropInt 0 lst = lst
dropInt n (x:xs) = dropInt (n - 1) xs

-- sumInt returns the sum of the items in a list.
sumInt :: Num a => [a] -> a
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs
-- scanSum adds the items in a list and returns a list of the running totals. So, scanSum [2,3,4,5] returns [2,5,9,14].
scanSum :: Num a => [a] -> [a]
scanSum lst = helper lst 0 where 
    helper [] _ = []
    helper (x:xs) acc = (acc + x) : helper xs (acc + x)

-- diffs returns a list of the differences between adjacent items. So, diffs [3,5,6,8] returns [2,1,2]. (Hints: one solution involves writing an auxiliary function which takes two lists and calculates the difference between corresponding elements. Alternatively, you might explore the fact that lists with at least two elements can be matched to a (x:y:ys) pattern.)
diffs :: Num a => [a] -> [a]
diffs [x] = []
diffs [x1, x2] = [x1 - x2]
diffs (x1:x2:xs) = (x1 - x2) : diffs  (x2:xs)

-- Use map to build functions that, given a list xs of Ints, return:
--     A list that is the element-wise negation of xs.
negateList :: [Int] -> [Int]
negateList = map ((*) (-1))

--     A list of lists of Ints xss that, for each element of xs, contains the divisors of xs. You can use the following function to get the divisors:
divisors p = [ f | f <- [1..p], p `mod` f == 0 ]
allDivisors :: [Int] -> [[Int]]
allDivisors = map divisors

--     The element-wise negation of xss.
negateListOfLists :: [[Int]] -> [[Int]]
negateListOfLists = map negateList

-- Implement a Run Length Encoding (RLE) encoder and decoder.
--     The idea of RLE is simple; given some input:

--     "aaaabbaaa"

--     compress it by taking the length of each run of characters:(4,'a'), (2, 'b'), (3, 'a')
--     The concat and group functions might be helpful. In order to use group, import the Data.List module by typing :m Data.List at the ghci prompt or by adding import Data.List to your Haskell source code file.
--     What is the type of your encode and decode functions?
myEncode :: String -> [(Int, Char)]
myEncode x = zip (map length $ group x) (map head $ group x) 

myDecode :: [(Int, Char)] -> String
myDecode = concat . map (uncurry replicate)

-- With respect to your solutions to the first set of exercises in this chapter, is there any difference between scanSum (takeInt 10 [1..]) and takeInt 10 (scanSum [1..])?
-- ScanSum TakeInt gives running total of 1 to 10. TakeInt ScanSum never terminates... if it wasnt lazy. Its lazy so no dfiference.

-- Write functions that, when applied to lists, give the last element of the list and the list with the last element dropped (without using reverse).
-- This functionality is provided by Prelude through the last and init functions. Like head and tail, they blow up when given empty lists.
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Define the following functions recursively (like the definitions for sum, product and concat above), then turn them into a fold:
--     and :: [Bool] -> Bool, which returns True if a list of Bools are all True, and False otherwise.
--     or :: [Bool] -> Bool, which returns True if any of a list of Bools are True, and False otherwise.
myAnd :: [Bool] -> Bool
myAnd [x] = x
myAnd (False:xs) = False
myAnd (True:xs) = myAnd xs

myFoldedAnd :: [Bool] -> Bool
myFoldedAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:xs) = True
myOr (False:xs) = myOr xs

myFoldedOr :: [Bool] -> Bool
myFoldedOr = foldr (||) False
-- Define the following functions using foldl1 or foldr1:
--     maximum :: Ord a => [a] -> a, which returns the maximum element of a list (hint: max :: Ord a => a -> a -> a returns the maximum of two values).
--     minimum :: Ord a => [a] -> a, which returns the minimum element of a list (hint: min :: Ord a => a -> a -> a returns the minimum of two values).
myMaximum :: Ord a => [a] -> a
myMaximum = foldl1 max

-- Use a fold (which one?) to define reverse :: [a] -> [a], which returns a list with the elements in reverse order.
myReverse :: [a] -> [a]
myReverse = foldl (\x y -> y:x) []

-- Write your own definition of scanr, first using recursion, and then using foldr. Do the same for scanl first using recursion then foldl.
myRecursiveScanl :: (a -> b -> a) -> a -> [b] -> [a]
myRecursiveScanl f starting lst = helper f lst [] where
    helper f [] res = res
    helper f (x:xs) [] = helper f xs [(f starting x)] 
    helper f (x:xs) res = helper f xs ((f (head res) x):res )

myFoldScanl :: (a -> b -> a) -> a -> [b] -> [a]
myFoldScanl f firstVal lst = reverse $ foldl (\acc x -> (f (head acc) x):acc) [firstVal] lst

myRecursiveScanr :: (a -> b -> b) -> b -> [a] -> [b]
myRecursiveScanr f acc [] = [acc]
myRecursiveScanr f acc (x:xs) = (f x (head temp)) : temp where
    temp = myRecursiveScanr f acc xs

myFoldScanr :: (a -> b -> b) -> b -> [a] -> [b]
myFoldScanr f firstVal lst = foldr (\x acc -> (f x $ head acc) : acc ) [firstVal] lst

factList :: Integer -> [Integer]
factList n = scanl1 (\acc x -> x * acc) [1..n]

-- Write a returnDivisible :: Int -> [Int] -> [Int] function which filters a list of integers retaining only the numbers divisible by the integer passed as first argument. For integers x and n, x is divisible by n if (mod x n) == 0 (note that the test for evenness is a specific case of that).
returnDivisible :: Int -> [Int] -> [Int]
returnDivisible m lst = [x | x <- lst, mod x m == 0]

-- Write a function choosingTails :: [[Int]] -> [[Int]] using list comprehension syntax with appropriate guards (filters) for empty lists returning a list of tails following a head bigger than 5:

-- choosingTails  [[7,6,3],[],[6,4,2],[9,4,3],[5,5,5]]
-- -- [[6,3],[4,2],[4,3]]
choosingTails :: [[Int]] -> [[Int]]
choosingTails lst = [xs | (x:xs) <- lst, x > 5]

-- Does the order of guards matter? You may find it out by playing with the function of the preceding exercise.
-- Over this section we've seen how list comprehensions are essentially syntactic sugar for filter and map. Now work in the opposite direction and define alternative versions of the filter and map using the list comprehension syntax.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p lst = [x | x <- lst, p x]

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = [f x| x<-lst]

-- Rewrite doubleOfFirstForEvenSeconds using filter and map instead of list comprehension.
doubleOfFirstForEvenSeconds :: [(Int, Int)] -> [Int]
doubleOfFirstForEvenSeconds ps =  map ((2*).fst) $ filter ((0==).(`mod` 2).snd) ps


main = putStrLn "Hello World"
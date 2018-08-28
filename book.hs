import Data.Char
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger where 
    smaller = [a | a <- xs, a <= x]
    bigger = [b | b <- xs, b > x]

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myN = div a (length xs) where
    a = 10
    xs = [1,2,3,4,5]

myLast :: [a] -> a
myLast = head .reverse

myInit = reverse . tail . reverse

myInit2 [] = error "Wutface"
myInit2 [x] = []
myInit2 (x:xs) = x : myInit2 xs

ch3_21 = [True]
ch3_22 = [[1,2]]
ch3_23 :: Int -> Int -> Int -> Int
ch3_23 x y z = x
ch3_24 x = (x,x)
ch3_25 x y = x y

-- second xs = head (tail xs) :: [a] -> a
-- swap (x,y) = (y,x) :: (a,b) -> (b,a)
-- pair x y = (x,y) :: a -> b -> (a,b)
-- double x = x*2 :: Int a => a -> a
-- palindrome xs = reverse xs == xs :: Eq a => [a] -> Bool
-- twice f x = f (f x) :: (a -> a) -> a -> a

second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
palindrome xs = reverse xs == xs
twice f x = f (f x)

halve list = splitAt ((length list) `div` 2) list

myThird = head . tail . tail
myThird2 list = list !! 2

myThird3 (_:_:x:xs) = x
myThird3 _ = error "Merpderp"

safetail list = if null list then list else tail list
safetail1 list | null list = list
                | otherwise = tail list
safetail2 [] = []
safetail2 list = tail list

myAnd a b = if a then (if b then True else False) else False
myAnd2 a b = if a then b else False

-- meaning of multiply 3 integers. (\x -> (\y -> (\z -> x*y*z)))

luhnDouble x    |  (*2) x >= 10 = ((*2) x) - 9
                | otherwise = 2 * x

luhn a b c d = ((luhnDouble a) + (luhnDouble c) + b + d) `mod` 10 == 0

foo = 5
f x = x + foo

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find x list = [v | (u,v) <- list, u == x]

find2 x list = helper list
    where helper [] = []
          helper ((a,b):xs) = if a == x then b : helper xs 
            else helper xs   

adjPairs xs = zip xs (tail xs)

testSorted xs = and [a <= b | (a,b) <-adjPairs xs]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (i + ord 'a')

shift :: Int -> Char -> Char
shift i c | isLower c = int2let $ (let2int c + i) `mod` 26
          | otherwise = c

encode n xs = [shift n x | x <- xs]

count x vs = length [v | v <- vs, v == x]

percent n m = (fromIntegral n )/ (fromIntegral m) * 100

freqs str = [percent (count x str) n | x <- ['a'..'z']] 
    where n = length [v | v <- str, isLower v]            

chisqr xs ys = sum [((x-y)^2)/y | (x,y) <- zip xs ys]

rotate :: Int -> [a] -> [a]
rotate x list = drop x list ++ take x list

positions k xs = [pos | (x,pos) <- zip xs [0..], x == k]

encodedString = "kdvnhoo lv ixq"
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
table' = freqs encodedString

crack xs = encode (-factor) xs where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

hundredSquares = [x^2 | x <- [1..100]]

grid m n = [(x,y) | x <- [0..m], y<-[0..n]]

square n= [(x,y) | (x,y) <- grid n n, x /= y]

myReplicate n val = [val | x <- [1..n]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors n = [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], sum (factors x) == x + x]

form1 = [(x,y) | x <- [1,2], y <- [3,4]]
form2 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]


positions2 k list = find k $ zip list [1..]

scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]
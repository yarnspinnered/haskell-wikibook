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
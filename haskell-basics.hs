
import System.Environment

r = 5.0
area r = pi * r ^ 2
triangle b h = 0.5 * b * h

-- Explain how GHCi evaluates quadruple 5.
-- quadruple 5 resolves to double (double 5)
-- double (2 * 5)
-- double 10
-- 2 * 10
-- 20
double x    = 2 * x
quadruple x = double (double x)

-- Define a function that subtracts 12 from half its argument.
half x = x / 2
subtract12 x = (half x) - 12

-- Write a function to calculate the volume of a box.
areaRect l w = l * w
volBox l w h = (areaRect l w) * h


-- Write a function to calculate the volume of a cylinder. The volume of a cylinder is the area of the base, which is a circle (you already programmed this function in this chapter, so reuse it) multiplied by the height.
volCylinder r h = (area r) * h

-- variables local to a function
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where
    s = (a + b + c) / 2

-- 2 == True errors out
-- Type mismatch. Values have types and they constrain what can be done with specific values.

-- guards. Drop the equal sign, indent and use booleans before equal sign
-- - isn't a unary function that negates a word. Thus the use of 0 - x
-- Otherwise is a symbol that has value true by default
absolute x
    | x < 0     = 0 - x
    | otherwise = x

-- Combine guards and wheres
numOfRealSolutions a b c
    | disc > 0  = 2
    | disc == 0 = 1
    | otherwise = 0
        where
        disc = b^2 - 4*a*c

-- The negate function, which takes an Int and returns that Int with its sign swapped. For example, negate 4 = -4, and negate (-2) = 2
negate1 ::Int -> Int
negate1 x = (-x)

-- The (||) function, pronounced 'or', that takes two Bools and returns a third Bool which is True if either of the arguments were, and False otherwise.
(||~) :: Bool -> Bool -> Bool
(||~) False False = False
(||~) x y = True

f :: Bool -> Bool -> Bool
f x y = not x && y

g :: Int -> Int
g x = (2*x - 1)^2

-- Would the following piece of Haskell work: 3:[True,False]? Why or why not?
-- No, its cons-ing a Num to a list of Bools

-- Write a function cons8 that takes a list as an argument and conses 8 (at the beginning) on to it. Test it out on the following lists by doing:
--     cons8 []
--     cons8 [1,2,3]
--     cons8 [True,False]
--     let foo = cons8 [1,2,3]
--     cons8 foo
cons8 :: [Int] -> [Int]
cons8 lst = 8 : lst

-- Adapt the above function in a way that 8 is at the end of the list. (Hint: recall the concatenation operator ++ from the previous chapter.)
append8 :: [Int] -> [Int]
append8 lst = lst ++ [8]

-- Write a function that takes two arguments, a list and a thing, and conses the thing onto the list. You can start out with:
myCons :: [a] -> a -> [a]
myCons list thing = thing : list

-- Which of these are valid Haskell and which are not? Rewrite in cons notation.
-- [1,2,3,[]] Invalid
-- [1,[2,3],4] Invalid
-- [[1,2,3],[]] Valid

-- Write down the 3-tuple whose first element is 4, second element is "hello" and third element is True.
-- (4, "hello", True)

-- Which of the following are valid tuples?
--     (4, 4) Valid
--     (4, "hello") Valid
--     (True, "Blah", "foo") Valid
--     () Valid

-- Lists can be built by consing new elements onto them. Cons a number onto a list of numbers, you will get back a list of numbers. There is no such way to build up tuples.
--     Why do you think that is? Tuple shave a fixed length.
--     For the sake of argument, say that there was such a function. What would you get if you "consed" something on a tuple? A new tuple with an additional element at the front. Differnt type.


-- Use a combination of fst and snd to extract the 4 from the tuple (("Hello", 4), True).
-- (snd. fst) (("Hello", 4), True)

-- Normal chess notation is somewhat different to ours: it numbers the rows from 1-8 and the columns a-h; and the column label is customarily given first. Could we label a specific point with a character and a number, like ('a', 4)? What important difference with lists does this illustrate?
-- We can for tuples because they accept different values in the same tuple.

-- Write a function which returns the head and the tail of a list as the first and second elements of a tuple.
listToTuple lst = (head lst, tail lst)

-- Use head and tail to write a function which gives the fifth element of a list. Then, make a critique of it, pointing out any annoyances and pitfalls you notice.
fifthElem lst = (head . tail . tail . tail . tail ) lst

-- The solution to the third exercise of the previous section ("... a function which returns the head and the tail of a list as the first and second elements of a tuple").
-- [a] -> (a, [a])

-- The solution to the fourth exercise of the previous section ("... a function which gives the fifth element of a list").
-- [a] -> a

-- h x y z = chr (x - 2) (remember we discussed chr in the previous chapter).
-- Int -> b -> c -> Char



-- Write a program which asks the user for the base and height of a right angled triangle, calculates its area, and prints it to the screen. The interaction should look something like:

-- The base?
-- 3.3
-- The height?
-- 5.4
-- The area of that triangle is 8.91

-- You will need to use the function read to convert user strings like "3.3" into numbers like 3.3 and the function show to convert a number into string.

-- main :: IO ()
-- main = do
--     putStrLn "The base?"
--     base <- getLine
--     putStrLn "The height?"
--     height <- getLine
--     putStrLn $ "The area of triangle is " ++ show (triangle (read base) (read height))

-- Write a program that asks the user for his or her name. If the name is one of Simon, John or Phil, tell the user that you think Haskell is a great programming language. If the name is Koen, tell them that you think debugging Haskell is fun (Koen Classen is one of the people who works on Haskell debugging); otherwise, tell the user that you don't know who he or she is. 
-- main :: IO ()
-- main = do
--     putStrLn "Your name"
--     name <- getLine
--     if name == "Simon" || name == "Phil" || name == "John"
--         then putStrLn "Haskell is great!"
--     else if name == "Koen"
--         then putStrLn "Debugging haskell is fun!"
--     else
--         putStrLn "I dont know who you are"

-- Why does the unsweet version of the let binding require an extra do keyword?
-- A let .. in ... thing has is typed as thing. Thus thing must be of type IO Monad, hence the do is required.

-- Do you always need the extra do?
-- Not if its a IO action.

-- (extra credit) Curiously, let without in is exactly how we wrote things when we were playing with the interpreter in the beginning of this book. Why is it ok to omit the in keyword in the interpreter but needed (outside of do blocks) in a source file?
-- The GHCI interpreter is a huge do-block that converts normal expressions to IO-actions.


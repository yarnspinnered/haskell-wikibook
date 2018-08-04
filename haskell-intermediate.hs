-- desugar

doGuessing num = do {
    putStrLn "Enter your guess:";
    guess <- getLine;
    case compare (read guess) num of {
        LT -> do {putStrLn "Too low!";
             doGuessing num;};
        GT -> do {putStrLn "Too high!";
                 doGuessing num;};
        EQ -> putStrLn "You Win!";
}
}
  
-- Enum. A datatype where none of the constructors have arguments.
data Month = January | February | March | April | May | June | July
           | August | September | October | November | December

-- Named fields. Useful when interested in one or two fields.
-- data Configuration = Configuration
--     String   -- User name
--     String   -- Local host
--     String   -- Remote host
--     Bool     -- Is guest?
--     Bool     -- Is superuser?
--     String   -- Current directory
--     String   -- Home directory
--     Integer  -- Time connected
--   deriving (Eq, Show)

-- -- Accessors
-- getUserName (Configuration un _ _ _ _ _ _ _) = un
-- getLocalHost (Configuration _ lh _ _ _ _ _ _) = lh
-- getRemoteHost (Configuration _ _ rh _ _ _ _ _) = rh
-- getIsGuest (Configuration _ _ _ ig _ _ _ _) = ig
-- But this is annoying, especially if we modify this datatype in the future to add something in front


-- Use named fields instead
data Configuration = Configuration 
    { username      :: String
    , localHost     :: String
    , remoteHost    :: String
    , isGuest       :: Bool
    , isSuperuser   :: Bool
    , currentDir    :: String
    , homeDir       :: String
    , timeConnected :: Integer
    } deriving (Show)
-- Automatically generates getters
-- username :: Configuration -> String
-- localHost :: Configuration -> String
-- And so on...

-- Generate a new cfg with one modified field
changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir = cfg { currentDir = newDir }

-- Pattern matching on named fields
-- getHostData (Configuration { localHost = lh, remoteHost = rh }) = (lh, rh)

-- Constructing new data

initCFG = Configuration "nobody" "nowhere" "nowhere" False False "/" "/" 0

initCFG' = Configuration
    { username      = "nobody"
    , localHost     = "nowhere"
    , remoteHost    = "nowhere"
    , isGuest       = False
    , isSuperuser   = False
    , currentDir    = "/"
    , homeDir       = "/"
    , timeConnected = 0
    }

-- Type parameters. The type Maybe is either Nothing or a Just a where a is variable.
-- data Maybe a = Nothing | Just a

-- data Either a b = Left a | Right b

pairOff :: Int -> Either String Int
pairOff people
    | people < 0  = Left "Can't pair off negative number of people."
    | people > 30 = Left "Too many people for this activity."
    | even people = Right (people `div` 2)
    | otherwise   = Left "Can't pair off an odd number of people."

groupPeople :: Int -> String
groupPeople people =
    case pairOff people of
        Right groups -> "We have " ++ show groups ++ " group(s)."
        Left problem -> "Problem! " ++ problem

-- Kind errors (Errors in the parameterized types)

-- Data structure time!
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f = g where
    g (Leaf x) = Leaf (f x)
    g (Branch left right) = Branch (g left) (g right)

treeFold :: (b -> b -> b) -> (a -> b) -> (Tree a) -> b
treeFold fbranch fleaf  = g where
    g (Leaf x) = fleaf x
    g (Branch l r) = fbranch (g l) (g r)

tree1 :: Tree Integer
tree1 = 
    Branch
       (Branch 
           (Branch 
               (Leaf 1) 
               (Branch (Leaf 2) (Leaf 3))) 
           (Branch 
               (Leaf 4) 
               (Branch (Leaf 5) (Leaf 6)))) 
       (Branch
           (Branch (Leaf 7) (Leaf 8)) 
           (Leaf 9))
 
doubleTree = treeMap (*2)  -- doubles each value in tree
sumTree = treeFold (+) id -- sum of the leaf values in tree
fringeTree = treeFold (++) (: [])  -- list of the leaves of tree

data Weird a b = First a
    | Second b
    | Third [(a,b)]
    | Fourth (Weird a b)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g where
    g (First x) =  First (fa  x)
    g (Second x) = Second (fb x)
    g (Third lst) = Third [(fa x, fb y) | (x,y) <- lst]
    g (Fourth x) = Fourth (g x)

-- weirdFold :: (a -> c) -> (b -> c) -> ([a,b] -> c) -> (c -> c) -> Weird a b -> c
-- weirdFold f1 f2 f3 f4 = g where
--     g (First x) = f1 x
--     g (Second x) = f2 x
--     g (Third x) = f3 x
--     g (Fourth x) = f4 (g x)

-- Type classes are a set of classes. Used to limit the set of type variables
-- How to create a new type class
-- class <ClassName> a where
    -- fn1, fn2 with specific type signatures exist
    -- Can define one in term of the others so when creating a new instance of the class, only need define one
class NewClass a where
    classMethod1, classMethod2 :: a -> Bool
    classMethod1 x = not (classMethod2 x)
    classMethod2 x = not (classMethod1 x)
-- To make a new instance of this class
-- First create a type
-- Declare this type belongs to the class and define the required methods
data Foo = Foo {
    truth :: Bool
}

instance NewClass Foo where
    classMethod1 x = truth x

-- Deriving boilerplate code
-- Eq 
--     Equality operators == and /=

-- Ord 
--     Comparison operators < <= > >=; min, max, and compare.

-- Enum 
--     For enumerations only. Allows the use of list syntax such as [Blue .. Green].

-- Bounded 
--     Also for enumerations, but can also be used on types that have only one constructor. Provides minBound and maxBound as the lowest and highest values that the type can take.

-- Show 
--     Defines the function show, which converts a value into a string, and other related functions.

-- Read 
--     Defines the function read, which parses a string into a value of the type, and other related functions.

-- class  (Num a, Ord a) => Real a  where
--     -- | the rational equivalent of its real argument with full precision
--     toRational          ::  a -> Rational

-- A class can inherit from another class via the => operator when defining the class. To inherit frm more than one, do (parent1, parent2) => NewClass a where

    -- Using multiple constraints
    -- foo :: (Num a, Show a, Show b) => a -> a -> b -> String

instance Functor Tree where
    fmap f (Leaf x)            = Leaf   (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

doubleTreeFunctor :: (Functor f, Num b) => f b -> f b
doubleTreeFunctor = fmap (*2)
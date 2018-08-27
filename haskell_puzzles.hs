
myLast :: [a] -> a
myLast list = 
    case list of
        [] -> error "No end for empty lists"
        [x] -> x
        x : xs -> myLast xs

myButLast :: [a] -> a
myButLast [] = error "No second last item in empty list"
myButLast [x] = error "No second last item in list with only one item"
myButLast (x:xs) = if length xs == 1 then
    x else myButLast xs

elementAt :: [a] -> Integer -> a
elementAt (x:xs) 0 = x
elementAt (x:xs) k = elementAt xs $ k - 1

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse list = reverse' [] list where
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs
    
main = do {
    putStrLn "Input is always [1,2,3,4]";
    putStrLn "myLast";
    print $ myLast [1,2,3,4];
    -- print $ myLast ([]::[Integer]);
    putStrLn "myButLast";
    print $ myButLast [1,2,3,4];
    putStrLn "element at index 1";
    print $ elementAt [1,2,3,4] 1;
    putStrLn "Length";
    print $ myLength [1,2,3,4];
    putStrLn "Reverse";
    print $ myReverse [1,2,3,4];
}
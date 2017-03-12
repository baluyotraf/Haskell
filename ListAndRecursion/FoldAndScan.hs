import Data.List

--Applies and operation in all elements in the list
fs_and :: [Bool] -> Bool
fs_and [] = error "fs_and: empty list"
fs_and list = foldr (&&) True list

--Applies or operation in all elements in the list
fs_or :: [Bool] -> Bool
fs_or [] = error "fs_or: empty list"
fs_or list = foldr (||) False list

--Gets the maximum value from a list
fs_maximum [] = error "fs_maximum: empty list"
fs_maximum list = foldr1 higher list
    where
        higher a b
            | a > b = a
            | otherwise = b

--Gets the minimum value from a list
fs_minimum [] = error "fs_minimum: empty list"
fs_minimum list = foldr1 lower list
    where
        lower a b
            | a < b = a
            | otherwise = b

--Reverses a list
fs_reverse [] = []
fs_reverse list = foldr append [] list
    where
        append a b = b ++ [a]

--Recursive implementation of scanr
rec_scanr :: (a -> b -> b) -> b -> [a] -> [b]
rec_scanr f acc list = rec_scanr' f [acc] list
    where
        rec_scanr' f acc [] = acc
        rec_scanr' f aa@(a:as) list = rec_scanr' f newTotal newList
            where
                newList = init list
                newTotal = ((f.last) list a):aa

--Fold implementation of scanr
fold_scanr :: (a -> b -> b) -> b -> [a] -> [b]
fold_scanr f acc list = foldr f' [acc] list
    where
        f' element xx@(x:xs) = (f element x):xx

--Recursive implementation of scanl
rec_scanl :: (b -> a -> b) -> b -> [a] -> [b]
rec_scanl f acc list = rec_scanl' f [acc] list
    where
        rec_scanl' _ acc [] = acc
        rec_scanl' f acc (x:xs) = rec_scanl' f newAcc newList
            where
                newAcc = acc ++ [(f.last) acc x]
                newList = xs

--Fold implementation of scanl
fold_scanl :: (b -> a -> b) -> b -> [a] -> [b]
fold_scanl f acc list = foldl' f' [acc] list
    where
        f' acc element = acc ++ [(f.last) acc element]

--Gets all factorials from the given number
factList :: Integer -> [Integer]
factList i
    | i < 0 = error "factList: cannot be less than 0"
    | i == 0 = [1]
    | otherwise = scanl1 (*) [1..i]

--Returns all numbers divisible by a number from a given list
returnDivisible :: Int -> [Int] -> [Int]
returnDivisible i arr = [ x | x <- arr, x `mod` i == 0]

--Returns the tails if the head is greater than
choosingTails :: [[Int]] -> [[Int]]
choosingTails arr = [ xs | (x:xs) <- arr, x > 5 ]

--List comprehension impementation of the filter function
fs_filter :: (a -> Bool) -> [a] -> [a]
fs_filter f arr = [ x | x <- arr, f x ]

--List comprehension implementation of the map function
fs_map :: (a -> b) -> [a] -> [b]
fs_map f arr = [ f x | x <- arr ]

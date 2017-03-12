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
        rec_scanr' f (a:as) list = rec_scanr' f newTotal newList
            where
                newList = init list
                newTotal = ((f.last) list a):a:as

--Fold implementation of scanr
fold_scanr :: (a -> b -> b) -> b -> [a] -> [b]
fold_scanr f acc list = foldr f' [acc] list
    where
        f' element (x:xs) = (f element x) : x : xs

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

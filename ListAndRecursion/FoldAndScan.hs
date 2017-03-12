--Applies and operation in all elements in the list
fs_and :: [Bool] -> Bool
fs_and [] = error "fs_and: empty list"
fs_and list = foldr (&&) True list

fs_or :: [Bool] -> Bool
fs_or [] = error "fs_or: empty list"
fs_or list = foldr (||) False list

fs_maximum [] = error "fs_maximum: empty list"
fs_maximum list = foldr1 higher list
    where
        higher a b
            | a > b = a
            | otherwise = b

fs_minimum [] = error "fs_minimum: empty list"
fs_minimum list = foldr1 lower list
    where
        lower a b
            | a < b = a
            | otherwise = b

fs_reverse [] = []
fs_reverse list = foldr append [] list
    where
        append a b = b ++ [a]

rec_scanr :: (a -> b -> b) -> b -> [a] -> [b]
rec_scanr f acc list = rec_scanr' f acc list []
    where
        rec_scanr' f acc [] total = acc:total
        rec_scanr' f acc list total = rec_scanr' f newAcc newList newTotal
            where
                newAcc = f (last list) acc
                newList = init list
                newTotal = acc:total

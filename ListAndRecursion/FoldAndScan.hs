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

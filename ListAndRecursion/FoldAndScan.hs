--Applies and operation in all elements in the list
and :: [Bool] -> Bool
and [] = error "and: empty list"
and list = foldr (&&) True list

or :: [Bool] -> Bool
or [] = error "or: empty list"
or list = foldr (||) False list

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

--Applies and operation in all elements in the list
getAnd :: [Bool] -> Bool
getAnd [] = error "getAnd: empty list"
getAnd list = foldr (&&) True list

getOr :: [Bool] -> Bool
getOr [] = error "getOr: empty list"
getOr list = foldr (||) False list

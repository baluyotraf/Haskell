--Takes n number of elements from an interger array
takeInt :: Int -> [Int] -> [Int]
takeInt _ [] = []
takeInt 0 _ = []
takeInt n (x:xs) = x:(takeInt (n-1) xs)

--Removes n number of elements from an integer array
dropInt :: Int -> [Int] -> [Int]
dropInt _ [] = []
dropInt 0 list = list
dropInt n (x:xs) = dropInt (n-1) xs

--Get the sum of elements in an integer array
sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt(xs)

--Gets the running totals
scanSum :: [Int] -> [Int]
scanSum [] = []
scanSum list = scanSum' 0 list
    where
    scanSum' _ [] = []
    scanSum' n (x:xs) = x+n:(scanSum' (x+n) xs)

--Gets the difference between the items
diffs :: [Int] -> [Int]
diffs [] = []
diffs (x:[]) = []
diffs (x:y:ys) = (y-x):(diffs (y:ys))

--Negates all elements in an integer list
negateIntList :: [Int] -> [Int]
negateIntList list = map negate list

--Gets all the divisors from an integer list
getDivisors :: [Int] -> [[Int]]
getDivisors list  = map divisors list
    where
    divisors n = [f | f <- [1 .. n], n `mod` f == 0]

--Gets all the negated divisors from an integer list
getNegatedDivisors :: [Int] -> [[Int]]
getNegatedDivisors = map negateIntList . getDivisors

--Gets the last element of an integer list
getLast :: [Int] -> Int
getLast [] = error "getLast: empty list"
getLast [x] = x
getLast (_:xs) = getLast(xs)

--Gets the starting elements of an integer list without the last
getInit :: [Int] -> [Int]
getInit [] = error "getInit: empty list"
getInit [x] = []
getInit (x:xs) = x:(getInit xs)

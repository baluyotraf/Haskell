--Takes n number of elements from an interger array
takeInt :: Int -> [Int] -> [Int]
takeInt _ [] = []
takeInt 0 _ = []
takeInt n (x:xs) = x:(takeInt (n-1) xs)

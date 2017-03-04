messageMap :: String -> String
messageMap n
    | n `elem` ["Simon", "John", "Phil"] = "Haskell is a great programming language"
    | n == "Koen" = "Haskell debugging is fun"
    | otherwise = "Name unrecognized"

nameExercise = do
    putStr "Enter your name: "
    name <- getLine
    let message = messageMap name
    putStrLn message

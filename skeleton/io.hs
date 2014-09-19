-- Input and Output in Haskell
------------------------------
-- Structure of an IO program:
-- f :: IO a
-- f = do
--   x <- action1
--   action2 x
--   y <- action3
--   action4 x y

-- In an IO block, each consecutive line will have an IO supertype

-- Example: Ask a user to enter a list of numbers. Print the sum of the numbers
toList :: String -> [Integer]
toList input = read ("[" ++ input ++ "]")

main = do
    putStrLn "Enter a list of numbers (separated by comma):"
    input <- getLine
    print $ sum (toList input)
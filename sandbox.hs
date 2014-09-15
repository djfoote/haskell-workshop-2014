square :: Int -> Int
square x = x * x

sumSquares :: Int -> Int -> Int
sumSquares x y = square x + square y

squareSum :: Int -> Int -> Int
squareSum x y = square (x + y)

bigness :: Int -> String
bigness x = if x > 50 then "big" else "small"

myMood :: String -> String
myMood sky = (if sky == "blue" then 'r' else 's') : "ad"
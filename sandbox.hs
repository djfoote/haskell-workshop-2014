square x = x * x
sumSquares x y = square x + square y
squareSum x y = square (x + y)
bigness x = if x > 50 then "big" else "small"
myMood sky = (if sky == "blue" then 'r' else 's') : "ad"
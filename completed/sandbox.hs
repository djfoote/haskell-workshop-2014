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

data Complex = Complex Double Double deriving (Show, Eq)

magnitude :: Complex -> Double
magnitude (Complex re im) = (re**2 + im**2)**(0.5)

real :: Complex -> Double
real (Complex re _) = re

imag :: Complex -> Double
imag (Complex _ im) = im
-- =============================== Functions ================================ --

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

-- ======================= Complex Numbers Data Type ======================== --

data Complex = Complex Double Double deriving (Show, Eq)

magnitude :: Complex -> Double
magnitude (Complex re im) = (re**2 + im**2)**(0.5)

real :: Complex -> Double
real (Complex re _) = re

imag :: Complex -> Double
imag (Complex _ im) = im

addComplex :: Complex -> Complex -> Complex
addComplex (Complex a1 b1) (Complex a2 b2) = Complex (a1 + a2) (b1 + b2)

multComplex :: Complex -> Complex -> Complex
multComplex (Complex a1 b1) (Complex a2 b2) = 
    Complex (a1 * a2 - b1 * b2) (a1 * b2 + b1 * a2)

-- ==================== Pattern Matching and Recursion ====================== --

element :: (Eq a) => a -> [a] -> Bool
element _ []     = False
element x (y:ys) = if x == y then True 
                   else element x ys

listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

butLast :: [a] -> [a]
butLast [x]    = []
butLast (x:xs) = x : butLast xs
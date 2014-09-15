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

-- Standard Example: Binary Trees
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a) deriving (Show)

-- Deriving creates the mentioned functions for the class. In this case, deriving creates the Show (to_string) function for the binary tree node class

-- Function to create a tree from a list:
-- Creates a tree where first element is the tree node, all elements of list less than first element are on left side, rest are on right side
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))

treeMain = print $ treeFromList [7,2,4,8]
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a) deriving (Show)

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))

treeMain = print $ treeFromList [7,2,4,8]
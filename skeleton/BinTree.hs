data BinTree a = Empty
               | Node a (BinTree a) (BinTree a) deriving (Show)

















t1 = Node 3 
        (Node 6 Empty Empty) 
        (Node 2 
              (Node 4 Empty Empty) 
              Empty)

t2 = bstFromList [7,2,4,8,1]

--                   ======================================                   --
--                   == THIS IS YOUR ABSTRACTION BARRIER ==                   --
--                   ======================================                   --

-- You can only make a BST out of orderable types. Here we nub duplicates.
bstFromList :: (Ord a) => [a] -> BinTree a
bstFromList [] = Empty
bstFromList (x:xs) = Node x (bstFromList (filter (<x) xs))
                            (bstFromList (filter (>x) xs))

-- Uncomment the following block if you want pretty-printed trees.
-- If you do, delete "deriving (Show)" from above.
-- I'm not going to explain this, but I invite you to figure it out after
--     you've read about typeclasses somewhere else. It's kinda cool.
{--
instance (Show a) => Show (BinTree a) where 
    show tree = 
        let indent x = replicate (x * 2) ' '
            treeToString _ Empty = ""
            treeToString depth (Node x left right) = 
                indent depth ++ (show x) ++ "\n" 
                ++ treeToString (depth + 1) left 
                ++ treeToString (depth + 1) right
        in  init $ treeToString 0 tree
--}

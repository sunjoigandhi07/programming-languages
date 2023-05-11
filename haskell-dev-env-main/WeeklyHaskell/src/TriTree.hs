module TriTree where
    data TriTree a = Empty | 
                 Leaf a | 
                 Node a a (TriTree a) (TriTree a) (TriTree a) 
                 deriving (Eq,Show)
    -- search 
    -- consumes a value and a TriTree
    -- produces a Bool, T if the value is stored in the tree, F if not 
    search :: Ord a => a -> TriTree a -> Bool 
    search _ Empty = False 
    search val (Leaf a) = val == a
    search val (Node a b left middle right)
        | val == a || val == b = True
        | val < a = search val left 
        | val > b = search val right 
        | otherwise = search val middle 
    
    -- insert 
    -- consumes a value and a TriTree 
    -- produces a new TriTree with that value now stored 
    insert :: Ord a => a -> TriTree a -> TriTree a 
    insert val Empty = Leaf val 
    insert val (Leaf a) 
        | val < a = Node val a (Leaf val) Empty Empty 
        | val > a = Node a val (Leaf a) Empty Empty 
        | otherwise = Leaf a 
    insert val (Node a b left middle right) 
        | val < a = Node val b (insert val left) middle right 
        | val > b = Node a b left middle (insert val right)
        | val == a || val == b = Node a b left middle right 
        | val > a && val < b = Node a b left (insert val middle) right 
        | otherwise = error "Cannot insert value into TriTree"

    -- insertList 
    -- consumes a list of values and a TriTree 
    -- produces a new TriTree with all the values from the given list 
    insertList :: Ord a => [a] -> TriTree a -> TriTree a 
    insertList fs tree = foldr insert tree fs

    -- identical 
    -- consumes two TriTrees 
    -- produces a Bool, T if the trees are identical, F if not 
    identical :: Eq a => TriTree a -> TriTree a -> Bool 
    identical Empty Empty = True 
    identical (Leaf a) (Leaf b) = a == b
    identical (Node a1 a2 l1 l2 l3) (Node b1 b2 r1 r2 r3) = 
        a1 == b1 && a2 == b2 &&
        identical l1 r1 && identical l2 r2 && identical l3 r3
    identical _ _ = False

    -- treeMap 
    -- consumes a function and a TriTree 
    -- produces a new tree with the function (a -> b) applied to every value stored 
    treeMap:: (a -> b) -> TriTree a -> TriTree b 
    treeMap _ Empty = Empty 
    treeMap t (Leaf x) = Leaf (t x)
    treeMap t (Node x y left mid right) = Node (t x) (t y) (treeMap t left) (treeMap t mid) (treeMap t right)

    -- treeFoldPreOrder 
    -- consumes a function, an initial value, and a TriTree 
    -- produces the result of using the function to combine values in the TriTree 
    treeFoldPreOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldPreOrder t val Empty = val
    treeFoldPreOrder t val (Leaf x) = t val x
    treeFoldPreOrder t val (Node x y left mid right) =
        let
            val1 = t (treeFoldPreOrder t val left) (treeFoldPreOrder t val mid)
            val2 = t (treeFoldPreOrder t val right) y
        in
            t (t val1 x) val2

    -- treeFoldInOrder 
    -- consumes a function, an initial value, and a TriTree 
    -- produces the result of using the function to combine values in the TriTree 
    treeFoldInOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldInOrder t val Empty = val
    treeFoldInOrder t val (Leaf x) = t val x
    treeFoldInOrder t val (Node x y left mid right) =
        let
            val1 = t (treeFoldInOrder t val left) x
            val2 = t (treeFoldInOrder t val mid) y
            val3 = t (treeFoldInOrder t val right) val
        in
            t (t val1 val2) val3

    -- treeFoldPostOrder 
    -- consumes a function, an initial value, and a TriTree 
    -- produces the result of using the function to combine values in the TriTree 
    treeFoldPostOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldPostOrder t val Empty = val
    treeFoldPostOrder t val (Leaf x) = t val x
    treeFoldPostOrder t val (Node x y left mid right) =
        let
            val1 = t (treeFoldPostOrder t val left) (treeFoldPostOrder t val mid)
            val2 = t (treeFoldPostOrder t val right) val
        in
            t (t val1 val2) x

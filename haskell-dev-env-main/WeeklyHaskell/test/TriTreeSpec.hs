{-# LANGUAGE BlockArguments #-}
module TriTreeSpec where 
    import Test.Hspec 
    import TriTree

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "search " $ do 
            let tree = Node 4 6 (Leaf 1) (Leaf 5) (Leaf 7)
            let t = Empty
            it "produces a T/F value of T" $ 
                search 7 tree `shouldBe` True
            it "produces a T/F value of F" $
                search 9 tree `shouldBe` False 
            it "produces a T/F value of F  " $
                search 9 t `shouldBe` False
        describe "insert" $ do 
            it "produces a new tree with the value included" $ 
                insert 10 Empty `shouldBe` Leaf 10
            it "produces a new tree with the value included" $
                insert 2 (Leaf 3)`shouldBe` Node 2 3 (Leaf 2) Empty Empty 
            it "produces a new tree with the value included" $
                insert 4 (Leaf 4)`shouldBe` Leaf 4
        describe "insertList" $ do 
            it "produces a new TriTree with the given value included" $
                insertList [3, 1, 5, 4, 6, 2] Empty `shouldBe` Node 1 6 (Node 1 2 (Leaf 1) Empty Empty) (Node 3 5 (Node 3 4 (Leaf 3) Empty Empty) Empty Empty) Empty
            it "produces a new TriTree with the given value included" $
                let t = Node 1 6 (Node 1 3 (Leaf 1) (Leaf 2) (Node 3 5 Empty (Leaf 4) (Leaf 5))) Empty (Leaf 6)
                in insertList [0, 7, 4, 8, 2, 9] t `shouldBe` Node 0 6 (Node 0 3 (Node 0 1 (Leaf 0) Empty Empty) (Leaf 2) (Node 3 5 Empty (Leaf 4) (Leaf 5))) (Node 2 4 (Leaf 2) Empty Empty) (Node 6 9 (Leaf 6) (Node 7 8 (Leaf 7) Empty Empty) Empty)
            it "produces a new TriTree with the given value included" $
                insertList [3, 1, 5, 3, 2, 5, 4, 1, 6] Empty `shouldBe`  Node 1 6 (Leaf 1) (Node 2 5 (Node 2 4 (Leaf 2) Empty Empty) (Leaf 3) Empty) Empty 
        describe "identical" $ do 
            let tree1 = Node 3 4 (Leaf 1) (Leaf 2) Empty 
                tree2 = Node 3 4 (Leaf 1) (Leaf 2) Empty 
                tree3 = Node 3 4 (Leaf 1) (Leaf 7) Empty 
                tree4 = Node 3 4 (Leaf 1) (Leaf 2) (Leaf 7)
            it "produces T/F if the TriTrees are identical" $
                identical tree1 tree2 `shouldBe` True
            it "produces T/F if the TriTrees are identical" $
                identical tree1 tree4 `shouldBe` False
            it "produces T/F if the TriTrees are identical" $ 
                identical tree3 tree4 `shouldBe` False 
        describe "treeMap" $ do 
            let tree1 = Node 1 2 (Leaf 3) Empty (Leaf 4)
                tree2 = Node "1" "2" (Leaf "3") Empty (Leaf "4")
                tree3 = Empty
            it "produces a new tree" $ do
                treeMap (+1) Empty `shouldBe` Empty
            it "produces a new tree" $ do
                treeMap show tree1 `shouldBe` Node "1" "2" (Leaf "3") Empty (Leaf "4")
            it "produces a new tree" $ do
                treeMap (+1) tree3 `shouldBe` Empty
        describe "treeFoldPreOrder" $ do
            let tree1 = Node 5 6 (Leaf 2) (Leaf 3) (Node 4 1 (Leaf 7) Empty Empty)
                tree2 = Node 3 6 (Leaf 7) (Node 5 1 Empty (Leaf 9) Empty) (Leaf 2)
                tree3 = Node 1 2 (Node 3 4 (Leaf 5) (Leaf 6) (Leaf 7)) (Node 8 9 (Leaf 10) (Leaf 11) (Leaf 12)) (Node 13 14 (Leaf 15) (Leaf 16) (Leaf 17))
            it "produces a tree with combined values" $ do
                treeFoldPreOrder (+) 0 (Leaf 5) `shouldBe` 5
            it "produces a tree with combined values" $ do
                treeFoldPreOrder (*) 1 tree1 `shouldBe` 5040
            it "produces a tree with combined values" $ do
                treeFoldPreOrder (-) 0 tree2 `shouldBe` -7
        describe "treeFoldInOrder" $ do
            let tree = Node 1 5 (Leaf 2) (Leaf 3) (Leaf 4)
            it "produces a tree with combined values" $ do
                treeFoldInOrder (+) 0 tree `shouldBe` 15
            it "produces a tree with combined values" $ do
                treeFoldInOrder (*) 1 tree `shouldBe` 120
            it "produces a tree with combined values" $ do
                treeFoldInOrder (-) 0 tree `shouldBe` 9
        describe "treeFoldPostOrder" $ do
            let tree1 = Node 2 3 (Leaf 1) Empty (Leaf 4)
                tree2 = Node 5 10 (Leaf 2) (Leaf 3) (Leaf 4)
                tree3 = Node 3 5 (Leaf 1) Empty (Leaf 4)
            it "produces a tree with combined values" $
                treeFoldPostOrder (+) 0 (Leaf 1) `shouldBe` 1
            it "produces a tree with combined values" $
                treeFoldPostOrder (+) 0 tree1 `shouldBe` 7
            it "produces a tree with combined values" $
                treeFoldPostOrder (+) 0 tree2 `shouldBe` 14



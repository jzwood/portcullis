data BinaryTree a = Leaf | Node { node :: a
                                , leftBranch :: BinaryTree a
                                , rightBranch :: BinaryTree a
                                } deriving (Eq)

instance Show a => Show (BinaryTree a) where
  show Leaf = "null"
  show (Node v bL bR)
    =  [show v, show bL, show bR]
    & intercalate ", "
    & bracket

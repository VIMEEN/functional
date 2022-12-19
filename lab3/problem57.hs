import System.IO

data BinarySearchTree = Null | Node Int (BinarySearchTree) (BinarySearchTree)
  deriving Show

insert Null val = Node val Null Null
insert (Node x left right) val
  | x > val = Node x left' right
  | otherwise = Node x left right'
  where
  left' = insert left val
  right' = insert right val

fromList xs = foldl insert Null xs

toPreorderArray Null = []
toPreorderArray (Node x left right) = toPreorderArray left ++ [x] ++ toPreorderArray right

main = do
  print $ tree1
  print $ tree2
  print $ toPreorderArray tree2
  where
  a = insert Null 7
  b = insert a 3
  tree1 = insert b 9
  tree2 = fromList [4,1,2,3,45,6,2,4]

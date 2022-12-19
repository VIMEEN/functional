data Tree x = Nil | Node x (Tree x) (Tree x) deriving (Show, Eq)

countLeaves Nil = 0
countLeaves (Node t Nil Nil) = 1
countLeaves (Node t l r) = countLeaves l + countLeaves r

main::IO()
main = putStr$ show$ countLeaves (Node 1 (Node 2 Nil (Node 4 Nil Nil)) (Node 2 Nil Nil))

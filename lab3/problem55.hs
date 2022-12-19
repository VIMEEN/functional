data Tree = Nil | Node Char Tree Tree deriving (Show, Eq)

dop a b
      | a == b = [(b,b)]
      | otherwise = [(a,b),(b,a)] 
      
cbalTree 0 = [Nil]
cbalTree 1 = [Node 'x' Nil Nil]
cbalTree 2 = [Node 'x' Nil (Node 'x' Nil Nil), Node 'x' (Node 'x' Nil Nil) Nil]
cbalTree n = [Node 'x' l r | (k1,k2) <- dop a1 a2, l <-cbalTree k1, r <- cbalTree k2] where
    a1 = div t 2
    a2 = div t 2 + mod t 2
    t = n-1

main = print $ show (cbalTree 2)

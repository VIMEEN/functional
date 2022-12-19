module Main (main) where

main :: IO ()
main = do
    print $ myLength ""
    print $ myLength "abcde"
    print $ myLength [1, 2, 3]

myLength :: [a] -> Int
myLength = foldr (const (+1)) 0

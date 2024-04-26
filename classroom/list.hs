concatList :: [a] -> [a] -> [a]
concatList [] l = l
concatList (x:xs) l = x : concatList xs l

takeNFirstElements :: (Num n, Eq n) => n -> [a] -> [a]
takeNFirstElements 0 _ = []
takeNFirstElements n (x:xs) = x : takeNFirstElements (n - 1) xs

mapValues :: (a -> b) -> [a] -> [b]
mapValues _ [] = []
mapValues f (x:xs) = f x : mapValues f xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList f (x:xs)
    | f x = x : c
    | otherwise = c
    where c = filterList f xs

main :: IO ()
main = do
    print (concatList [1, 2, 3] [4, 5, 6])
    print (takeNFirstElements 3 [1, 2, 3, 4, 5])
    print (mapValues (\x -> x * 2) [1, 2, 3, 4, 5])
    print (filterList (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5])

-- Infix functions

-- In Haskell, we can define functions that use infix notation. To do this, we need to use backticks (`) to wrap the function name.

-- For example, we can define a function that verify if a element contains in a list:

contains :: Eq a => a -> [a] -> Bool
_ `contains` [] = False
x `contains` (y:ys) = x == y || x `contains` ys
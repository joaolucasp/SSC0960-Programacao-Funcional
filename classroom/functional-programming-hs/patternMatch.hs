-- Pattern Matching is a way to match values against patterns and, if the value matches the pattern, execute a block of code.
-- The syntax is similar to guards, but instead of using the pipe character (|), we use the equal sign (=).
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

patternMatchConditional :: Int -> String
patternMatchConditional 0 = "zero"
patternMatchConditional x
    | x < 0 = "negative"
    | x > 0 = "positive"

fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n - 1)

sumList :: [Int] -> Int
sumList [] = 0
sumList list = head list + sumList (tail list)

sumList2 :: [Int] -> Int
sumList2 [] = 0
sumList2 (x:xs) = x + sumList2 xs

sumPairs :: [Int] -> [Int]
sumPairs [] = []
sumPairs [x] = [x]
sumPairs (x:y:xs) = (x + y) : sumPairs xs

mergeSort :: [Int] -> [Int] -> [Int]
mergeSort [] ys = ys
mergeSort xs [] = xs
mergeSort (x:xs) (y:ys)
    | x < y = x : mergeSort xs (y:ys)
    | otherwise = y : mergeSort (x:xs) ys

main :: IO ()
main = do
    print (sumPairs [1, 2, 3, 4, 5])
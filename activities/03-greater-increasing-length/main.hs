-- Encontra todos os segmentos crescentes em uma lista
increasing :: [Int] -> [[Int]]
increasing [] = []
increasing [x] = [[x]]
increasing (x:y:rest)
    | x < y     = let (h:t) = increasing (y:rest)
                  in (x:h):t
    | otherwise = [x] : increasing (y:rest)

-- Encontra o comprimento do maior segmento crescente
maxIncreasing :: [Int] -> Int
maxIncreasing = maximum . map length . increasing

main :: IO ()
main = do
    input <- getLine
    let numbers = map read (words input) :: [Int]
    print (maxIncreasing numbers)
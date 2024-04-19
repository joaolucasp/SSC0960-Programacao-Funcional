readXAndY :: IO (Int, Int)
readXAndY = do
    x <- readLn
    y <- readLn
    return (x, y)

isPrime :: Int -> Bool
isPrime number = number > 1 && all (\x -> number `mod` x /= 0) [2..(number-1)]

nextConsecutivePrimeNumber :: Int -> Int
nextConsecutivePrimeNumber n
    | isPrime n = n
    | otherwise = nextConsecutivePrimeNumber (n + 1)

primeIntervalsLength :: Int -> Int -> Int
primeIntervalsLength x y
    | prime2 > y = 0
    | otherwise = calculateInterval prime2 (nextConsecutivePrimeNumber (prime2 + 1)) (prime2 - prime1)
    where
        calculateInterval prime1 prime2 maxInterval
            | prime2 > y = maxInterval
            | interval > maxInterval = calculateInterval prime2 (nextConsecutivePrimeNumber (prime2 + 1)) interval
            | otherwise = calculateInterval prime2 (nextConsecutivePrimeNumber (prime2 + 1)) maxInterval
            where interval = prime2 - prime1

        prime1 = nextConsecutivePrimeNumber x
        prime2 = nextConsecutivePrimeNumber (prime1 + 1)

main :: IO ()
main = do
    (x, y) <- readXAndY
    print (primeIntervalsLength x y)
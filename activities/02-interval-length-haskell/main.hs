readXAndY :: IO (Int, Int)
readXAndY = do
    x <- readLn
    y <- readLn
    return (x, y)

isPrime :: Int -> Bool
isPrime number = number > 1 && all (\x -> number `mod` x /= 0) [2..(number-1)]

nextConsecutivePrimeNumber :: Int -> Int
nextConsecutivePrimeNumber n = if isPrime n then n else nextConsecutivePrimeNumber (n + 1)

primeIntervalsLength :: Int -> Int -> Int
primeIntervalsLength x y =
    let calculateInterval prime1 prime2 maxInterval =
            if prime2 > y
                then maxInterval
                else let interval = prime2 - prime1
                    in if interval > maxInterval
                        then calculateInterval prime2 (nextConsecutivePrimeNumber (prime2 + 1)) interval
                        else calculateInterval prime2 (nextConsecutivePrimeNumber (prime2 + 1)) maxInterval

        prime1 = nextConsecutivePrimeNumber x
        prime2 = nextConsecutivePrimeNumber (prime1 + 1)

        in if prime2 > y
            then 0
            else calculateInterval prime2 (nextConsecutivePrimeNumber (prime2 + 1)) (prime2 - prime1)


main :: IO ()
main = do
    (x, y) <- readXAndY
    print (primeIntervalsLength x y)
-- Guards are a way to define functions that have multiple conditions
guardsConditional :: Int -> String
guardsConditional x
    | x < 0 = "negative"
    | x == 0 = "zero"
    | x > 0 = "positive"
    | otherwise = "undefined"

imc :: Float -> Float -> String
imc weight height
    | imcValue < 18.5 = "Underweight"
    | imcValue < 24.9 = "Normal weight"
    | imcValue < 29.9 = "Overweight"
    | otherwise = "Obesity"
    where imcValue = weight / (height * height)

bhaskara :: Float -> Float -> Float -> [Float]
bhaskara a b c
    | delta < 0 = []
    | delta == 0 = [(-b) / (2 * a)]
    | otherwise = [((-b) + sqrt delta) / (2 * a), ((-b) - sqrt delta) / (2 * a)]
    where delta = b * b - 4 * a * c

main :: IO ()
main = do
    x <- readLn
    putStrLn (guardsConditional x)
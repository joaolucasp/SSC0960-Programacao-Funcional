reduce :: (b -> a -> a) -> a -> [b] -> a
reduce _ base [] = base 
reduce op base (x:xs) = op x $ reduce op base xs

{- Como aplicar o reduce para a função soma -}
soma = reduce (+) 0

{- Como aplicar o reduce para a função multiplicação -}
produto = reduce (*) 1

{- Como aplicar o reduce para a função concatenar duas listas de string -}
concatenar = reduce (++) ""

{- Como aplicar o reduce para a função conc que vimos na ultima aula -}
conc [] l2 = l2
conc (x:xs) l2 = x:(conc xs l2)

concReduce l1 l2 = reduce (:) l2 l1

{- Como aplicar o reduce para a função map -}
mapa f l = reduce ((:).f) l

{- Como aplicar o reduce para a função fatorial -}
fat n = reduce (*) 1 [1..n]

-------------------------------------------------------------------------------
{- main = do
    putStrLn $ show $ primeiro [2,3]
    putStrLn $ either id show $ primeiro [] -} 
        
{- Função que retorna o primeiro elemento de uma lista -}
primeiro :: (Show a) => [a] -> Either a String
primeiro [] = Right "Nao ha elementos"
primeiro (x:_) = Left x

{- main = do
    putStrLn $ show $ maximo ([] :: [Integer])
    putStrLn $ show $ maximo ([2,3,4] :: [Integer]) -}
    
maximo :: (Ord a) => [a] -> Maybe a
maximo [] = Nothing {- Construtor do maybe, que não da nenhum valor -}
maximo [x] = Just x {- Construtor de valor do maybe -}
maximo (x:xs) = case maximo xs of
    Nothing -> Just x
    Just y -> Just $ if x > y then x else y
    
-------------------------------------------------------------------------------
-- main = do
    --putStrLn $ show $ x {- Para e fala que está em loop -}
    -- putStrLn $ show $ y
    -- putStrLn $ show $ maximoRadical ([] :: [Integer]) {- Para o programa -}
    -- putStrLn $ show $ s
    
x = x + 1 {- Definir pode, ele compila. Quando usar que daria problema -}
y = 1:y {- Lista infinita de 1 -}

maximoRadical [] = undefined {- Não consegue tratar quando não tem valor definido, ele para -}
maximoRadical [x] = x
maximoRadical (x:xs) = if x > y then x else y
    where 
    y = maximoRadical xs

s = let
        ss n = n:ss (n+1)
    in  
        ss 1
        
--------------------------------------------------------------------------------------
{- main = do
    putStrLn $ show $ take 10 $ primos
    putStrLn $ show $ sum $ takeWhile (<1000) primos {- Soma dos primos menores que mil -}
    putStrLn $ show $ dropWhile (<50) $ takeWhile (<100) primos {- Primos entre 50 e 100 -} -}

primos = let
            b (x:xs) = x:(b (filter ((/=0).(`mod` x)) xs))
        in
            b [2..]

-- (`mod` x) -> aplica o mod cabeça
-- (/=0) -> o mod tem que ser diferente de zero

-- Exemplo de como funciona:
-- b [2,3,4,5,6,7,8,9,10,11]
-- 2:b (filter (/=0).(`mod` 2) [3,4,5,6,7,8,9,10,11])
-- 2:b ([3,5,7,9,11])
-- 3:b (filter (/=0).(`mod` 3) [3,5,7,9,11])
-- 3:b ([5,7,11])
-- 5:b (filter (/=0).(`mod` 5) [5,7,11])
-- 5:b ([7,11])

--------------------------------------------------------------------------------------
main = do
    putStrLn $ show $ take 10 $ iter 1 (+1)
    putStrLn $ show $ take 10 $ iter 1 (*2) {- Potencia de 2 -}
    putStrLn $ show $ sum $ take 10 $ iter 1 (*2) {- Soma das 10 primeiras potencias de 2 -}
    putStrLn $ show $ sum $ takeWhile (<1000000) $ iter 1 (*3)
    {- A leitura fica mais clara, mais fácil entender o que faz -}

iter b f = b:(iter (f b) f)

 
-- putStrLn $ show $ sum $ map (valorCompra) $ filter (>10) $ compras
{- Mais facil tentar entender qual o comando nessa linha -}
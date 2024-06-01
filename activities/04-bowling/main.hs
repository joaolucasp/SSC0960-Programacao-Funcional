import Data.List (intercalate)

-- Para facilitar a leitura do código, defini os seguintes tipos:
type Roll = Int -- Definição do tipo Roll, que representa uma jogada.
type Frame = [Roll] -- Definição do tipo Frame, que representa uma lista de jogadas (rodada).

main :: IO ()
main = do
    line <- getLine
    let input = (map read $ words line :: [Int])
    let (frames, totalPoints) = calculateBowlingScore input

    -- A função intercalate concatena a lista de frames (resultado de cada jogada), separadando pelo caractere "|".
    putStrLn $ intercalate " | " frames ++ " | " ++ show totalPoints


-- Essa função é a principal. Basicamente, ela recebe uma lista de jogadas e retorna uma tupla contendo a lista de frames (resultado de cada jogada) e a pontuação total.
calculateBowlingScore :: [Roll] -> ([String], Int)
calculateBowlingScore shots = (getFramesResult frames, totalPoints)
  where
    (frames, totalPoints) = processFrames shots

-- Essa função é responsável por processar as jogadas e calcular a pontuação total.
processFrames :: [Roll] -> ([Frame], Int)
processFrames frames = processFramesHelper frames 1 [] 0
  where
    -- Caso base: se a lista de jogadas for vazia, retorna uma tupla contendo a lista de frames vazia e a pontuação total obtida na recurssão.
    processFramesHelper [] _ frames total = (frames, total)

    -- Caso recursivo: para cada jogada, é avaliado caso a caso.
    processFramesHelper rolls frameIndex frames total
        -- Caso 1: se a jogada for um STRIKE, adiciona 10 pontos e o bônus da próxima jogada.
        | frameIndex > 10 = (frames, total)

        -- Caso 2: se a jogada for um SPARE, adiciona 10 pontos e o bônus da próxima jogada.
        | frameIndex == 10 = (frames ++ [takeExtraFrame rolls], total + sum (takeExtraFrame rolls))

        -- Caso 3: se a jogada for um OPEN FRAME, adiciona a pontuação da jogada.
        | isStrike rolls = processFramesHelper (tail rolls) (frameIndex + 1) (frames ++ [[10]]) (total + 10 + strikeBonus rolls)

        -- Caso 4: se a jogada for um SPARE, adiciona 10 pontos e o bônus da próxima jogada.
        | isSpare rolls = processFramesHelper (drop 2 rolls) (frameIndex + 1) (frames ++ [take 2 rolls]) (total + 10 + spareBonus rolls)

        -- Caso 5: se não for nenhum dos casos acima, adiciona a pontuação da jogada.
        | otherwise = processFramesHelper (drop 2 rolls) (frameIndex + 1) (frames ++ [take 2 rolls]) (total + sum (take 2 rolls))

    -- Essa é uma função auxiliar que verifica se a jogada é um STRIKE, verificando se a primeira jogada é igual a 10.
    isStrike (x:_) = x == 10
    isStrike _ = False

    -- Essa é uma função auxiliar que verifica se a jogada é um SPARE, verificando se a soma das duas primeiras jogadas é igual a 10.
    isSpare (x:y:_) = x + y == 10
    isSpare _ = False

    strikeBonus (x:y:z:_) = y + z
    strikeBonus _ = 0

    spareBonus (_:y:z:_) = z
    spareBonus _ = 0

    takeExtraFrame (x:y:z:_) = [x, y, z]
    takeExtraFrame lst = lst

-- Essa função é responsável por receber uma lista de frames (jogada) e, para cada um, retorna a pontuação de cada jogada (retornando a string correspondente).
getFramesResult :: [Frame] -> [String]
getFramesResult = map score
  where
    -- Caso 1: se a lista de jogadas tiver apenas um STRIKE, retorna "X _"
    score [10] = "X _"

    -- Caso 2: se a lista de jogadas tiver apenas duas jogadas, ele pode ser um SPARE ou um OPEN FRAME.
    score [x, y]
        | x + y == 10 = show x ++ " /"
        | otherwise = show x ++ " " ++ show y

    -- Caso 3: se a lista de jogadas tiver três jogada (última rodada), é avaliado caso a caso
    score [x, y, z]
        -- 3.0: Caso todas as jogadas sejam STRIKES, retorna "X X X"
        | x == 10 && y == 10 && z == 10 = "X X X"

        -- 3.1: Caso a primeira jogada seja um STRIKE && a segunda jogada NÃO SEJA um SPARE, retorna "X Y Z"
        | x == 10 && y + z < 10 = "X " ++ show y ++ " " ++ show z

        -- 3.2: Caso a primeira jogada seja um STRIKE && a segunda jogada SEJA um SPARE, retorna "X Y /"
        | x == 10 = "X " ++ show y ++ " /"

        -- 3.3: Caso a primeira jogada NÃO SEJA um STRIKE && a segunda jogada SEJA um SPARE, retorna "X / Z"
        | x + y == 10 = show x ++ " / " ++ show z
        | otherwise = show x ++ " " ++ show y ++ " " ++ show z
        
    -- Caso base
    score _ = ""
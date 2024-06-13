import Control.DeepSeq
import Control.Exception
import Data.List
import System.IO

-- Country = (Name, Confirmed, Deaths, Recovered, Active)
type CountryData = (String, Int, Int, Int, Int)

-- Essa função deve receber o caminho de um arquivo e retornar uma lista de tuplas com os dados dos países
readCSVFile :: FilePath -> IO [CountryData]
readCSVFile path = do
  h <- openFile path ReadMode
  contents <- hGetContents h

  -- O haskell é laziness, portanto, é necessário realizarmos alguma operação com a variável 'contents', para forçar a leitura do arquivo antes que ele seja fechado.
  evaluate $ force contents

  hClose h
  return $ map readLine $ lines contents

-- Essa função recebe uma String (linha de um arquivo) e retornar uma tupla com os dados do país
readLine :: String -> CountryData
readLine line = case splitString line of
  [name, confirmed, deaths, recovered, active] -> (name, read confirmed, read deaths, read recovered, read active)
  _ -> error $ "Invalid line: " ++ line

-- Função que divide uma string por vírgulas
splitString :: String -> [String]
splitString [] = [""]
splitString (c : cs)
  | c == ',' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitString cs

main :: IO ()
main = do
  -- Leitura dos valores n1, n2, n3 e n4
  input <- getLine
  let [n1, n2, n3, n4] = map read $ words input :: [Int]

  countries <- readCSVFile "dados.csv"

  -- 1) A soma de "Active" de todos os países em que "Confirmed" é maior o igual que n1.
  let activeSumWhenConfirmedIsGreatherOrEqualOne = getActiveSumWhenConfirmedIsGreatherOrEqualOne countries n1

  -- 2) Dentre os n2 países com maiores valores de "Active", a soma das "Deaths" dos n3 países com menores valores de "Confirmed".
  let deathsSum = getDeathsSum countries n2 n3

  -- 3) Os n4 países com os maiores valores de "Confirmed". Os nomes devem estar em ordem alfabética.
  let countriesWithHighestConfirmed = getCountriesWithHighestConfirmed countries n4

  print activeSumWhenConfirmedIsGreatherOrEqualOne
  print deathsSum
  mapM_ putStrLn countriesWithHighestConfirmed

getActiveSumWhenConfirmedIsGreatherOrEqualOne :: [CountryData] -> Int -> Int
getActiveSumWhenConfirmedIsGreatherOrEqualOne countryData n1 =
  let filteredCountryData = filter (\(_, confirmed, _, _, _) -> confirmed >= n1) countryData
   in sum [active | (_, _, _, _, active) <- filteredCountryData]

getDeathsSum :: [CountryData] -> Int -> Int -> Int
getDeathsSum countryData n2 n3 =
  let topN2ActiveCountries = take n2 $ sortByActive GT countryData
      countriesSortedByConfirmed = sortByConfirmed LT topN2ActiveCountries
      deathsValues = map extractDeaths $ take n3 countriesSortedByConfirmed
   in sum deathsValues

getCountriesWithHighestConfirmed :: [CountryData] -> Int -> [String]
getCountriesWithHighestConfirmed countryData n4 =
  let topN4ConfirmedCountries = take n4 $ sortByConfirmed GT countryData
      countryNames = map extractName $ sortAlphabetically topN4ConfirmedCountries
   in countryNames

-------------------------------------------------------
-- Funções auxiliares
-- OBS: LT representa uma ordenação ASCENDENTE e GT uma ordenação DESCENDENTE
-- Função genérica para ordenar por um campo específico
sortByField :: (Ord b) => Ordering -> (a -> b) -> [a] -> [a]
sortByField ord extractField = sortBy compareField
  where
    compareField x y = case ord of
      LT -> compare (extractField x) (extractField y)
      GT -> compare (extractField y) (extractField x)
      _ -> EQ

-- Funções específicas para cada campo
sortByActive :: Ordering -> [CountryData] -> [CountryData]
sortByActive ord = sortByField ord (\(_, _, _, _, active) -> active)

sortByConfirmed :: Ordering -> [CountryData] -> [CountryData]
sortByConfirmed ord = sortByField ord (\(_, confirmed, _, _, _) -> confirmed)

sortAlphabetically :: [CountryData] -> [CountryData]
sortAlphabetically = sortByField LT (\(name, _, _, _, _) -> name)

extractDeaths :: CountryData -> Int
extractDeaths (_, _, deaths, _, _) = deaths

extractName :: CountryData -> String
extractName (name, _, _, _, _) = name
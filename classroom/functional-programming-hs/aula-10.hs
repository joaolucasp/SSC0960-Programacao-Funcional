import Data.List (sortBy)
import Data.Ord (comparing)

data Person = Person { name :: String, age :: Int, course :: String } deriving (Show)
person1 = Person { name = "John", age = 30, course = "Computer Science" }
person2 = Person { name = "Jane", age = 25, course = "Information Systems" }

persons = [person1, person2]

main :: IO ()
main = do
    print (name person1)
    print (sum $ map age persons)
    print (filter (\x -> age x > 25) persons)
    print (sortBy (comparing age) persons)

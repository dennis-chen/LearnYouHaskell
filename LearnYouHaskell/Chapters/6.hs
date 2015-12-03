module Main where
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L

myTakeWhile :: (a->Bool)->[a]->[a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x = x:myTakeWhile p xs
    | otherwise = []

takeWhileFold :: (a->Bool)->[a]->[a]
takeWhileFold p = foldr(\x acc-> if p x then x:acc else []) []

countWords :: String -> [(String, Int)]
countWords = map (\ws -> (head ws, length ws)) . L.group . L.sort . words

countWords1 :: String -> [(String, Int)]
countWords1 x = foldr (\ws acc-> (head ws, length ws):acc) [] $ L.group . L.sort $ words x

isIn :: (Eq a) => [a]->[a]->Bool
xs `isIn` y = foldr (\x acc -> if xs `L.isPrefixOf` x then True else acc) False $ L.tails y

encode :: Int -> String -> String
encode shift = map (\x -> C.chr $ shift + C.ord x)

decode :: Int -> String -> String
decode shift = encode (-shift)

findNumWithSum :: Int -> Maybe Integer
--return first natural num where sum of digits equals target
findNumWithSum target = L.find (\x -> target == sumDigs x) [1..]
    where sumDigs n = L.foldl' (+) 0 $ map C.digitToInt $ show n

lookupKey :: (Eq a) => a -> [(a,b)] -> Maybe b
lookupKey k xs = case L.find (\(x,y) -> x == k) xs of 
                Just n -> Just $ snd n
                Nothing -> Nothing

lookupKey' :: (Eq a) => a -> [(a,b)] -> Maybe b
lookupKey' k = foldr (\(a,b) acc -> if k == a then Just b else acc) Nothing 

getAssocList :: [(String,String)]
getAssocList = [("john","111-111-1111"),("potato","123-123-1234")]

stringToDigits :: String -> [Int]
-- takes string and returns list of digits in string
stringToDigits = map C.digitToInt . filter C.isDigit

convertPBook :: [(String,String)] -> [(String,[Int])]
convertPBook = map (\(k,v) -> (k, stringToDigits v))

main = do 
    print $ getAssocList
    print $ convertPBook getAssocList

testArray = [1,1,2,2,3]
palArray = [3,2,1,1,2,3]

last' :: [a] -> a
last' = foldr1 (flip const)

lastButOne :: [a] -> a
lastButOne = last . init

kth :: [a] -> Int -> a
kth [] _ = error "empty list!"
kth (x:xs) k
  | k < 1 = error "invalid k!"
  | k == 1 = x
  | otherwise = kth xs (k-1)

len' :: [a] -> Int
len' = foldr (\x acc -> acc + 1) 0

rev :: [a] -> [a]
rev = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (rev xs)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

removeDups :: (Eq a) => [a] -> [a]
removeDups xs =
  foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs

pack :: (Eq a) => [a] -> [[a]]
pack = 
  foldr (\x acc -> let (firstSubList:rest) = acc in
    if firstSubList == []
    then [[x]]
    else if x == head firstSubList then (x:firstSubList):rest
    else [x]:acc) [[]]

runLengthEncode :: (Eq a) => [a] -> [(Int, a)]
runLengthEncode = map (\xs -> (length xs, head xs)) . pack

main = do
  print $ last testArray
  print $ lastButOne testArray
  print $ kth testArray 3
  print $ len' testArray
  print $ rev testArray
  print $ isPalindrome testArray
  print $ isPalindrome palArray
  print $ removeDups testArray
  print $ pack testArray
  print $ runLengthEncode testArray

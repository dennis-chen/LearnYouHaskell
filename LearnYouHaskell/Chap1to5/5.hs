module Main where

myZipWith :: (a->b->c)->[a]->[b]->[c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myFlip :: (a->b->c)->(b->a->c)
myFlip f x y = f y x

myMap :: (a->b)->[a]->[b]
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter :: (a->Bool)->[a]->[a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = x:myFilter f xs
    | otherwise = myFilter f xs

--find the largest number under 100,000 thats divisble by 3829
findLargest :: Int
findLargest = last (takeWhile (<100000) (map (*3829) [0..]))

findLargestTwo :: Int
findLargestTwo = head [x | x <- [100000,99999..], (x `mod` 3829) == 0]

--find the sum of all odd squares that are < 10,000
sumOdds :: Int
sumOdds = sum ([x^2 | x<-[1..100], odd x])

sumOdds1 :: Int
sumOdds1 = sum (filter odd (takeWhile (<10000) (map (^2) [1..])))

--for starting numbers between 1 and 100, how many collatz chains have
--a length greater than 15?
getCollatz :: Int->[Int]
getCollatz 1 = [1]
getCollatz x
    | even x = x:getCollatz (x `div` 2)
    | odd x = x:getCollatz ((x*3)+1)

findCollatz :: Int
findCollatz = length (filter (islong) (map (getCollatz) [1..100]))
    where islong xs = length xs > 15

--Implementation of sum using foldl
mySum :: Num a => [a] -> a
mySum = foldl (+) 0

--Implementation of map using foldr
myMap1 :: (a->b)->[a]->[b]
myMap1 f = foldr (\x xs-> (f x):xs) []

--elem using foldr
myElem :: (Eq a) => a -> [a] -> Bool
myElem e = foldr (\x accum-> accum || x == e) False

--max using foldr1
myMax :: (Ord a) => [a] -> a
myMax [] = error "Can't get max of an empty list!"
myMax xs = foldr1 (\x accum -> if x > accum then x else accum) xs

myReverse :: [a] -> [a]
myReverse = foldl (\accum x -> x:accum) []

myProduct :: (Num a) => [a] -> a
myProduct = foldr1 (*)

myFilter1 :: (a->Bool) -> [a] -> [a]
myFilter1 f = foldr (\x acc-> if f x then x:acc else acc) []

myLast :: [a] -> a
myLast = foldr1 (\_ acc->acc)

--how many elements does it take fo the sum of the square roots of all natural
--numbers to exceed 1,000?
findSumSqrGreater :: Int
findSumSqrGreater = length $ takeWhile (<1000) $ scanl (+) 0 $ map sqrt [1,2..]

--find sum of all odd squares less than 10,000
sumOdds2 :: Int
sumOdds2 = sum . takeWhile(<10000) $ map (^2) [1,3..]

main = do 
    print $ myFilter1 (odd) [1..6]
    print $ myLast [1..6]
    print $ findSumSqrGreater
    print $ sumOdd2

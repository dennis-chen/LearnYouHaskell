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
myMap1 f = foldr (\x xs-> (f x):xs)

main = do 
    print $ myZipWith (+) [1..5][2..]
    print $ myFlip (/) 2 6
    print $ (/) 2 6
    print $ take 5 (myMap (+10) [1..])
    print $ myFilter even [-5..5]
    print $ findLargest
    print $ findLargestTwo
    print $ sumOdds
    print $ sumOdds1
    print $ getCollatz 30
    print $ findCollatz
    print $ mySum [1..6]

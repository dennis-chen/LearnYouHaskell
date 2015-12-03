module Main where

boomBangs xs = [if x > 10 then "BANG" else "BOOM" | x <- xs, x `mod` 2 == 0]

test xs = [x | x <- xs, x /= 13, x /= 15, x/= 17]

allProducts xs ys = [x*y | x <- xs, y <-ys]

removeUpper s = [c | c <- s, c `elem` ['a'..'z']]

--take list of lists and remove odd numbers in inner lists w/o
--flattening list
removeInnerOdds xss = [[x | x <- xs, even x] | xs <- xss]

--return tuples of right triangle sides where length of each side <= 10
--and triangle perim is equal to 24
getTriangles = [(x,y,z) | z <- [1..10], y<-[1..z], x<-[1..y], 
    x^2 + y^2 == z^2, x+y+z == 24]

--recursively defined max function
mymax :: (Ord a) => [a] -> a
--TODO: this error message does not work for some reason
mymax [] = error "Tried to take max of an empty list!"
mymax (x:[]) = x
mymax (x:xs) = if x > prevMax then x else prevMax
                where prevMax = mymax(xs)

--recursively defined replicate function
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate x a
    | x > 0 = a:myreplicate (x-1) a
    | x < 0 = error "Can't replicate a negative num of times!"

--recursively defined take function
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake x (a:as)
    | x <= 0 = []
    | otherwise = a:mytake (x-1) as

--function application takes precedence over infix operators

--recursively defined reverse function
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

--repeat infinitely
myrepeat :: a -> [a]
myrepeat a = a:myrepeat a

--zip two lists togethers into list of tuples
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--return if value is elem in a list
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem e (x:xs)
    | x == e = True
    | otherwise = myelem e xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = quickSort [x | x <- xs, x <= p] ++ [p] ++ quickSort [x | x <- xs, x > p]

main = do 
    print $ mymax [9,8,3,10,3,4]
    print $ myreplicate 3 4
    print $ mytake 4 [1,2..]
    print $ myreverse [1,3..11]
    print $ mytake 5 (myrepeat 10)
    print $ myzip (myrepeat 1) ['A'..'Z']
    print $ myelem 3 [1,2,4]
    print $ myelem 3 [1,2,3]
    print $ quickSort [9,8,3,10,3,4]

module Main where

nums = [x | x <- [50..100], x `mod` 7 == 3]

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

main = do 
    print $ test [10..20]
    print $ take 16 (allProducts [1..] [1..])
    print $ removeUpper "AaBbCcDd"
    print $ removeInnerOdds [[1,3,5],[1,2,3,4],[2,4,6]]
    print $ getTriangles

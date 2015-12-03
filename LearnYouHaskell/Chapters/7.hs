module Main where

data Point = Point Float Float 
    deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = 3.14*r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

getConcentric :: [Shape]
getConcentric = map (Circle (Point 0 0)) [1..5]

nudge :: Shape->Float->Float->Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+b) (y2+b))

originCircle :: Float->Shape
originCircle = Circle (Point 0 0)

originRectangle :: Float->Float->Shape
originRectangle w h = Rectangle (Point 0 0) (Point w h)

data Person = Person { firstName :: String
                    , lastName :: String
                    , age :: Int
                    , height :: Float
                    , phoneNumber :: String
                    , flavor :: String } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

main = do 
    print $ "hello"
    print $ area $ Circle (Point 0 0) 5
    print $ area $ Rectangle (Point 0 0) (Point 4 5)
    print $ Rectangle (Point 0 0) (Point 4 5)
    print $ getConcentric
    print $ nudge (Circle (Point 0 0) 5) 3 5
    print $ originCircle 4
    print $ originRectangle 4 8
    print $ Person {firstName="a",lastName="b",age=3,height=0.5,phoneNumber="11",flavor="bleh"}
    print $ vplus (Vector 0 0 0.5) (Vector 1 2 3)

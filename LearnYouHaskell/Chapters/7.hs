module Main where
import qualified Data.Map as M

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

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Ord, Show, Read, Eq, Bounded, Enum)

--Lockers
data State = Empty | Taken deriving (Show)
type LockerNum = Int
type LockerCode = String
type ErrMsg = String
type Locker = (State, LockerCode)
type Lockers = M.Map Int Locker

getLockers :: Lockers
getLockers = M.fromList $ zip [1..20] $ zip (repeat Taken) (repeat "1234")

assignLocker :: Lockers->LockerNum->Either ErrMsg LockerCode
assignLocker l n = case M.lookup n l of
    Nothing -> Left "Couldn't find locker number!"
    Just (Taken,_) -> Left "Locker already taken!"
    Just (Empty,code) -> Right code

data Mylist a = Emptylist | Cons a (Mylist a) deriving (Show)

listToMylist :: [a] -> Mylist a
listToMylist = foldr Cons Emptylist

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

treeInsert :: (Ord a)=>a->Tree a->Tree a
treeInsert y EmptyTree = Node y EmptyTree EmptyTree
treeInsert y (Node x l r)
    | x <= y = Node x l (treeInsert y r)
    | x > y = Node x (treeInsert y l) r

listToTree :: (Ord a)=>[a]->Tree a
listToTree = foldr treeInsert EmptyTree

treeElem :: (Ord a)=>a->Tree a->Bool
treeElem x EmptyTree = False
treeElem x (Node y l r) = (x == y) || ((treeElem x l) || (treeElem x r))

class MyFunctor f where 
    myMap :: (a->b)-> f a -> f b

instance MyFunctor [] where 
    myMap = map

instance MyFunctor Tree where
    myMap f (EmptyTree) = EmptyTree
    myMap f (Node x l r) = Node (f x) (myMap f l) (myMap f r)

instance MyFunctor Maybe where
    myMap f Nothing = Nothing
    myMap f (Just x) = Just (f x)

instance MyFunctor (Either a) where
    myMap f (Left x) = Left x
    myMap f (Right x) = Right (f x)

main = do 
    print $ 1
    print $ myMap (+3) [1..5]
    print $ myMap (*2)  $ listToTree [1,4,2,3]
    print $ myMap (+1) $ Just 3
    print $ (Left 1 :: Either Int String)
    print $ myMap (++"blah") (Right "hi" :: Either Int String)

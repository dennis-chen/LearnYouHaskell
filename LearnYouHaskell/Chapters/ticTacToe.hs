module Main where
import qualified Data.Map as M

data Mark = X | O deriving (Show)

type Board = M.Map (Int,Int) (Maybe Mark)

initialBoard :: Board
initialBoard = foldr (\x accum->M.insert x Nothing accum) emptyBoard coords 
    where coords = [(x,y) | x<-[0..2], y<-[0..2]]
          emptyBoard = M.empty 

allCoords :: Board -> [Maybe (Maybe Mark)]
allCoords b = map ((flip M.lookup) b) coords
    where coords = [(x,y) | x<-[0..2], y<-[0..2]]

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

getRow :: Int -> Board -> [Maybe (Maybe Mark)]
getRow n b = map ((flip M.lookup) b) coords
    where coords = [(x,y) | x<-[n], y<-[0..2]]

getRowStr :: Int -> Board -> String
getRowStr n b = interleave "||||" (map markToString $ getRow n b)
    where markToString = (\x -> case (x) of Nothing -> ' '
                                            Just Nothing -> ' '
                                            Just (Just X) -> 'X'
                                            Just (Just O) -> 'O')

toStrings :: Board -> [String]
toStrings b = interleave (map ((flip getRowStr) b) [0..2]) (take 2 $ repeat "-------")

printBoard :: Board -> IO ()
printBoard b = mapM_ print $ toStrings b

main = do 
    printBoard initialBoard

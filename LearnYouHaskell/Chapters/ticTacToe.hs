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

toStrings :: (String a) => Board -> [a]
toString b = map spaceToStr $ allCoords b 
    where spaceToStr = (\x -> case (x) of Nothing -> ' '
                                          Just Nothing -> ' '
                                          Just (Just X) -> 'X'
                                          Just (Just O) -> 'O')

main = do 
    print $ toString $ initialBoard
    print $ toString $ M.insert (0,0) (Just X) initialBoard
    print $ allCoords initialBoard
    putStrLn $ toString initialBoard

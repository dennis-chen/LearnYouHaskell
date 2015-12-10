module Main where
import qualified Data.Map as M
import qualified Data.Char as C

data Mark = X | O deriving (Show,Eq)

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

type Err = String

parseCommand :: String -> Either Err (Int,Int)
parseCommand (x:',':y:[]) = if elem (a,b) validCoords 
    then Right (a,b)
    else Left "Invalid coordinates! Enter coords from 0 to 2!"
    where validCoords = [(x,y) | x<-[0..2],y<-[0..2]]
          a = C.digitToInt x
          b = C.digitToInt y
parseCommand _ = Left "Invalid format!"

getCommand :: IO (Int,Int)
getCommand = do 
             line <- getLine
             case parseCommand line of Right (x,y) -> return (x,y)
                                       Left err -> do
                                        print $ err
                                        getCommand

isValid :: (Int,Int)->Board->Bool
isValid c b = c `elem` map (\(k,v)->k) (filter (\(k,v) -> v==Nothing) $ M.toList b)


temp :: Board->[((Int,Int),Maybe Mark)]
temp b = filter (\(k,v) -> v == Nothing) $ M.toList b

placeMark :: Mark->Board->(Int,Int)->Board
placeMark m b c = M.insert c (Just m) b

playGame :: Board -> IO()
playGame b = do 
    printBoard b
    print "Enter coordinate where you would like to place your X as x,y"
    coords <- getCommand
    if isValid coords b then
        playGame (placeMark X b coords)
    else do
        print "Already taken!"
        playGame b

main = do 
    playGame initialBoard

module Main where

rev :: String->String
rev = foldl (flip (:)) ""

rev' :: String->String
rev' = foldr (\x accum -> accum ++ [x]) ""

myPutStr :: String -> IO ()
myPutStr "" = return ();
myPutStr (x:xs) = do 
    putChar x
    myPutStr xs

main = do 
    line <- getLine
    case line of
        "" -> return ()
        _ -> do
            putStrLn $ rev line
            main

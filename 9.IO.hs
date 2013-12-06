
import Control.Monad
import Data.Char
import System.IO
import System.Directory
import  Data.List
import System.Environment 
import System.Random

main = forever $ do
    gen <- getStdGen
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen')


main13 = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName 

main12 = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"  


main11 = do
    handle <- openFile "README.md" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle


main1 = do
    line <- getLine
    if null line
        then return ()
        else do
            let revLine = reverse line
            putStrLn revLine
            main1     
   

main2 = do     
    c <- getChar  
    when (c /= ' ') $ do  
            putChar c  
            main2


main3 = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors       
       

main4 = forever $ do 
    putStr "Give me some input"
    l<-getLine
    putStrLn $ map toUpper l 

main5 = do  
    contents <- getContents  
    putStr (map toUpper contents)


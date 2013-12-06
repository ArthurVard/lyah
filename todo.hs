import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")      

view :: [String] -> IO ()
view (fileName:_) = do
                contents <- readFile fileName
                let numberedTasks = zipWith (\n t -> show n ++ "-" ++ t) [0 ..] $ lines contents
                putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, strNum] = do
                        contents <- readFile fileName 
                        (tName, tHandle) <- openTempFile "." "temp"
                        let todolines = lines contents
                            number = read strNum 
                            newToDoList = delete (todolines !! number) todolines
                        hPutStr tHandle $ unlines newToDoList
                        hClose tHandle
                        removeFile fileName
                        renameFile tName fileName       




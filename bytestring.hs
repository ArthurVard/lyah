import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S  

import System.Environment  
import System.IO  
import System.IO.Error  
 
{--fch = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  
B.cons 85 $ B.pack [80,81,82,84]  
B.cons' 85 $ B.pack [80,81,82,84]  
foldr B.cons B.empty [50..60]  
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"  
Empty))))))))))  
foldr B.cons' B.empty [50..60]  
Chunk "23456789:;<" Empty  
--}

main1 = do  
    (fileName1:fileName2:_) <- getArgs  
    copyFile fileName1 fileName2  
  
copyFile :: FilePath -> FilePath -> IO ()  
copyFile source dest = do  
    contents <- B.readFile source  
    B.writeFile dest contents  

main = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e 

{--
get attributes form exception
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e = case ioeGetFileName e of 
        	Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
            Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e   
--}

{--The predicates that act on IOError are:
isAlreadyExistsError
isDoesNotExistError
isAlreadyInUseError
isFullError
isEOFError
isIllegalOperation
isPermissionError
isUserError

---
isUserError evaluates to True when we use the function 
userError to make the exception, which is used for making 
exceptions from our code and equipping them with a string. 
For instance, you can do ioError $ userError "remote computer 
unplugged!", although It's prefered you use types like Either 
and Maybe to express possible failure instead of throwing 
exceptions yourself with userError.
--}
import Data.List
import System.IO
import System.Environment

main = do
    [filein, fileout] <- getArgs
    handle <- openFile filein ReadMode
    out <- openFile fileout WriteMode
    contents <- hGetContents handle
    let wrds = words contents
    putStrLn "Введите слово, которое нужно заменить"
    x <- getLine
    putStrLn "Введите слово, на которое нужно заменить"
    y <- getLine
    let res = replace x y wrds
    hPutStr out res
    putStrLn res
    hClose handle
    hClose out

replace :: String -> String -> [String] -> String
replace x y [] = ""
replace x y (s:str) = if (s == x) then
             y ++ " " ++ (replace x y str)
             else
             s ++ " " ++ (replace x y str)

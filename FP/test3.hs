import Data.Char
transform :: String->String
transform str = map toUpper str
main = do
file <- readFile "test2.txt"
let str_lst = lines file 
let up_lst = map transform str_lst
writeFile "1.txt"(unlines up_lst)
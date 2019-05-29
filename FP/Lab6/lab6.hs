import System.IO  
import System.Directory  
import Data.List  
import Data.Char

main = do
    handle <- openFile "matrix.txt" ReadMode  
    contents <- hGetContents handle
    let matr = lines contents     
    
    outHandle <- openFile "out.txt" WriteMode     
    putStrLn "Here is your matrix:" 
    putStrLn $ unlines matr
  
    let matrix = tail matr
    let matrix1 = createPadding $ getIntMatrixFromLines matrix
    showIntMatrix matrix1
    let dimentions = words (matr !! 0)
    let rows1 = digitToInt $ head $ dimentions!!0
    let cols1 = digitToInt $ head $ dimentions!!1
    let resm = createAreaMatrixWith matrix1
    showIntMatrix (fst resm)
    let weight = (((fst resm) !! fst(snd resm)) !! snd(snd resm))
    putStrLn $ show ((fst weight) * (snd weight))
    hClose handle        
    hClose outHandle    
    return ()

type Area = (Int, Int)
type IntMatrix = [[Int]]
type SymbolicMatrix = [[String]]
type SymbolicRow = [String]
type IntRow = [Int]

getIntMatrixFromLines :: SymbolicRow -> IntMatrix
getIntMatrixFromLines (x:xs) = [getRowInInt $ words x] ++ getIntMatrixFromLines xs
getIntMatrixFromLines [] = []

getRowInInt :: SymbolicRow -> IntRow
getRowInInt [] = []
getRowInInt (r:row) = [read r :: Int] ++ getRowInInt row 

createPadding :: IntMatrix -> IntMatrix
createPadding [] = []
createPadding matrix = [take ((length $ matrix !! 0)+2) (repeat 2)] ++ sidePadding matrix ++ [take ((length $ matrix !! 0)+2) (repeat 2)]  

sidePadding :: IntMatrix -> IntMatrix
sidePadding [] = []
sidePadding (m:matrix) = [[2] ++ m ++ [2]] ++ sidePadding matrix

createAreaMatrixWith :: IntMatrix -> ([[Area]], Area)
createAreaMatrixWith m = let
                        formedRow m i j max_i max_j area_pos acc_row acc_area = let
                                new_element | (m!!i!!j /= 0) || (i + 1) >= max_i || (j + 1) >= max_j = (0, 0)
                                            | (m!!(i - 1)!!(j) == 1) || (m!!(i + 1)!!(j) == 1) || (m!!(i)!!(j - 1) == 1) || (m!!(i)!!(j + 1) == 1) = (0,0)
                                            | True = ((if i - 2 < 0 || j - 1 < 0 then 0 else fst (acc_area!!(i - 2)!!(j - 1))) + 1, (if j - 2 < 0 then 0 else snd (acc_row!!(j - 2))) + 1)
                                val_pos = fst area_pos
                                new_area_pos = if fst val_pos * snd val_pos < fst new_element * snd new_element then (new_element,(i - 1, j - 1)) else area_pos
                            in
                                if (j + 1) == max_j then (acc_row, area_pos) else formedRow m i (j + 1) max_i max_j new_area_pos (acc_row ++ [new_element]) acc_area
                        formedArea m i j max_i max_j area_pos acc_area = let
                                row = (formedRow m i j max_i max_j area_pos [] acc_area)
                            in
                                if (i + 1) == max_i then (acc_area, snd area_pos) else formedArea m (i + 1) j max_i max_j (snd row) (acc_area ++ [(fst row)])
                     in
                        formedArea m 1 1 (length m) (length (m!!0)) ((0,0),(0,0)) []

showIntMatrix [] = do
    return ()
showIntMatrix (m:matrix) = do
    putStrLn $ show m
    showIntMatrix matrix
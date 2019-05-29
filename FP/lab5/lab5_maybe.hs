import System.IO  
import System.Directory  
import Data.List  
import Data.Char  
import Data.List.Split

main = do
    handle1 <- openFile "db.txt" ReadMode  
    contents1 <- hGetContents handle1
    let objects = lines contents1     
    let db = parseObjects objects
    let p1 = getFromMaybe $ periodConstructor (Time 10) (Time 20)
    let p2 = getFromMaybe $ periodConstructor (Time 40) (Time 50)
    let p3 = getFromMaybe $ periodConstructor (Time 20) (Time 30)
    let p4 = getFromMaybe $ periodConstructor (Time 60) (Time 70)
    let d = [p2] ++ [p1]
    let x = diary_add d p3 0
    let rmv = diary_remove x 1
    let ob = [("Василий", [p1, p2]), ("Татьяна", [p3, p4])]
    outHandle <- openFile "out.txt" WriteMode
    db_interact db outHandle
    --showPeople (stringifyObjects db) 1
    hClose outHandle
    hClose handle1
    return ()

     
parseObjects :: [String] -> DB
parseObjects objects = let
                            create_db [] db = db
                            create_db (o:objects) db = let
                                    arr = words o
                                    diary_f [] diary = diary
                                    diary_f (a:arr) diary = let
                                            times = splitOn "-" a
                                        in
                                            diary_f arr (diary_add diary (getFromMaybe (periodConstructor (readTime (times !! 0)) (readTime (times !! 1)))) 0)
                                    db_r = (arr!!0, diary_f (tail arr) [])
                                in
                                    create_db objects (db ++ [db_r])
                       in
                            create_db objects []

stringifyObjects :: DB -> [String]
stringifyObjects (d:db) = reverse $ let
                           appendSecond ((Period t1 t2):xs) = (show t1) ++ "-" ++ (show t2) ++ " " ++ appendSecond (xs)
                           appendSecond [] = ""
                           create_string [] = []
                           create_string (d:db) = create_string db ++ [init ((fst d) ++ " " ++ (appendSecond $ snd d))]
                      in
                           create_string (d:db) 
                            
db_interact db outHandle = do
                let count = showPeople (stringifyObjects db) 1
                showPeople (stringifyObjects db) 1
                putStrLn ""
                putStrLn "Введите номера желаемых объектов через пробел"
                input <- getLine
                let numberList = let 
                               parseInput (i:input) = [((read i :: Int)-1)] ++ parseInput input
                               parseInput [] = []
                            in parseInput (words input)
                let list = let 
                               parseInput (i:input) = [fst $ db !! ((read i :: Int)-1)] ++ parseInput input
                               parseInput [] = []
                            in parseInput (words input)
                --putStrLn $ show list
                putStrLn ""
                putStrLn "Введите длину совещания"
                len <- getLine
                putStrLn "Введите минимальное время начала совещания в формате часы:минуты"
                time <- getLine
                --putStrLn time
                --putStrLn (show (read len :: Int))
                --putStrLn $ show $ (meeting (read len :: Int) (readTime time) list db)
                showPeriod $ getFromMaybe(meeting (read len :: Int) (readTime time) list db)
                
                let ob = let 
                        insertMeeting (man:people) (d:db) (n:numberList) period = if (fst d == man) then [(man,(diary_add (snd d) period 0))] ++ (insertMeeting people db numberList period)  else [d] ++ (insertMeeting (man:people) db (n:numberList) period)
                        insertMeeting [] (d:db) _ period = [d] ++ insertMeeting [] db [] period
                        insertMeeting [] [] [] period = []
                        dbSubstitute index db replacement = let (ys,zs) = splitAt index db in ys ++ replacement ++ (tail zs)
                             in insertMeeting list db numberList (getFromMaybe(meeting (read len :: Int) (readTime time) list db))
                --putStrLn $ show $ stringifyObjects ob
                exportDB outHandle (stringifyObjects ob)
                return (ob)

showPeople [] count = do
    return (count-1)
showPeople (p:people) count = do
    putStrLn $ (show count) ++ ") " ++ p 
    showPeople people (count+1)       
showPeriod (Period t1 t2) = do
        putStrLn ("Встреча назначена на период от " ++ show t1 ++ " до " ++ show t2)
 

exportDB outHandle [] = do
                    return ()
exportDB outHandle (l:list) = do
                    hPutStrLn outHandle l
                    exportDB outHandle list
                    
newtype Time = Time Int deriving (Ord, Eq)

instance Show (Time) where
    show (Time a) = convertTime a 0

convertTime a hoursCount =
            if a >= 60 then
                convertTime (a-60) (hoursCount+1)   
             else if a > 9 then
                    show (hoursCount+8) ++ ":" ++ show(a)
                  else show (hoursCount+8) ++ ":0" ++ show(a) 

readTime :: String -> Time             
readTime str = Time(x*60 + y - 8*60)
        where
            cnv = splitOn ":" (str)
            x = read (cnv !! 0) :: Int
            y = read (cnv !! 1) :: Int  
            
            
data Period = Period Time Time deriving (Show, Ord, Eq)

getFromMaybe m = case m of 
             Just a    ->  a
            
isNothing Nothing = True
isNothing _ = False
            

periodConstructor :: Time -> Time -> Maybe Period
periodConstructor (Time f) (Time s) | f > s = Nothing
                                    | otherwise = Just (Period (Time f) (Time s))

combine :: Period -> Period -> Maybe Period
combine (Period (Time f1) (Time s1)) (Period (Time f2) (Time s2)) | (f1 <= f2) && overlap (Period (Time f1) (Time s1)) (Period (Time f2) (Time s2)) = Just (Period (Time f1) (Time s2))
                                                                  | (f1 >= f2) && overlap (Period (Time f1) (Time s1)) (Period (Time f2) (Time s2)) = Just (Period (Time f2) (Time s1))
                                                                  | otherwise = Nothing
tryConnect :: Period -> Period -> Maybe Period
tryConnect (Period (Time f1) (Time s1)) (Period (Time f2) (Time s2)) | ((f1-1) == s2)  = Just (Period (Time f2) (Time s1))
                                                                  | ((f2-1) == s1) = Just (Period (Time f1) (Time s2))
                                                                  | otherwise = Nothing
before :: Period -> Period -> Bool
before (Period f1 (Time s1)) (Period (Time f2) s2) | s1 < f2 = True
                                                   | otherwise = False
                                                   
overlap :: Period -> Period -> Bool
overlap (Period (Time f1) (Time s1)) (Period (Time f2) (Time s2)) | (f2 <= s1 && f2 >= f1) || (f1 <= s2 && f1 >= f2) = True
                                                                  | otherwise = False
                                                                  
type Diary = [Period]
              
diary_add diary period count
    | length diary == 0 = [period]
    | (length diary) == count = diary ++ [period]
    | before (diary !! count) period    = diary_add diary period (count+1)
    | otherwise                         = let 
                                              cnt = if (overlap (diary !! count) period) then (count+1) else count
                                              (ys,zs) = splitAt (cnt) diary 
                                             in ys ++ [period] ++ zs

diary_add_and_combine diary period count
    | length diary == 0 = [period]
    | (length diary) == count = diary ++ [period]
    | before (diary !! count) period    = diary_add_and_combine diary period (count+1)
    | otherwise                         = let 
                                              cnt = if (overlap (diary !! count) period) then count+1 else count
                                              (ys,zs) = splitAt (cnt) diary 
                                              actualPeriod = if (isNothing(combine (diary !! count) period) == False) then [getFromMaybe(combine (diary !! count) period)] else [(diary !! count),period] 
                                             in (init ys) ++ actualPeriod ++ zs
  
diary_remove diary index = let (ys,zs) = splitAt index diary in ys ++ (tail zs)



findTime :: Int -> Time -> Diary -> Maybe Period
findTime len (Time time) (x:diary) = let
            findTime' len time t1 (Period (Time a) (Time b)) []
                    | time <= t1 && t1 + len - 1 <= a - 1 = Just (Period (Time t1) (Time (t1 + len - 1)))
                    | a == 721 = Nothing
                    | True = findTime' len time (if b + 1 < time then time else b + 1) (Period (Time (721)) (Time 0)) []
            findTime' len time t1 (Period (Time a) (Time b)) (x:diary)
                    | time <= t1 && t1 + len - 1 <= a - 1 = Just (Period (Time t1) (Time (t1 + len - 1)))
                    | True = findTime' len time (if b + 1 < time then time else b + 1) x diary
        in
            findTime' len time time x diary
                              

combineDiaries :: Diary -> Diary -> Diary
combineDiaries x [] = x
combineDiaries x (y:ys) = let
                             combineDiaries' x [] = diary_add_and_combine x y 0          
                             combineDiaries' x (y:ys) = combineDiaries' (diary_add_and_combine x y 0) ys
                          in   
                            combineDiaries' x (y:ys) 

type Person = String
type DB = [(Person, Diary)]

meeting :: Int -> Time -> [Person] -> DB -> Maybe Period
meeting len time (man:people) (d:db) = let
                                          oneDiary (man:people) (d:db) = let
                                                                            diaryList (man:people) (d:db) = if (fst d == man) then [snd d] ++ (diaryList people db) else diaryList (man:people) db
                                                                            diaryList [] _ = []
                                                                            diaryList _ [] = []
                                                                            combineDiaries' (d1:diaries) = combineDiaries d1 (combineDiaries' diaries)
                                                                            combineDiaries' [] = []
                                                                         in
                                                                            combineDiaries' (diaryList (man:people) (d:db))
                                                                            
                                       in
                                          findTime len time (oneDiary (man:people) (d:db))
                                     
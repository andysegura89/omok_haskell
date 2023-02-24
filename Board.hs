--Andres Segura
--80376343
--Dr.Cheon
--Programming Languages
--11-18-22
module Board where
import Prelude hiding (take, drop)

--given n, returns nxn 2d array
mkBoard :: Int -> [[Int]]
mkBoard n = mkBoardH n n

--helper for mkBoard, creates nxn array
--x : used as a counter
--n : nxn = size of array
mkBoardH :: Int -> Int -> [[Int]]
mkBoardH x n
  | x == 1 = [[(j - j) | j <- [1..n]]]
  | otherwise = [(j-j) | j <- [1..n]] : mkBoardH (x-1) n

--returns n in a nxn array
size :: [[Int]] -> Int
size bd = (length (head bd))

--returns row y in a 1 based indexed 2d array
row :: Int -> [[Int]] -> [Int]
row y bd
  | y == 1 = (head bd)
  | otherwise = row (y-1) (tail bd)

--returns col x in a 1 based indexed 2d array
col :: Int -> [[Int]] -> [Int]
col x bd
  | (length bd) == 1 = [getCol x (head bd)]
  | otherwise = getCol x (head bd) : col x (tail bd)

--gets value at index x of a 1 based indexed 1d array
getCol :: Int -> [Int] -> Int
getCol x bd 
  | x == 1 = (head bd)
  | otherwise = getCol (x-1) (tail bd) 

--gets diagonal going from top left to bottom right
getDownDiagonal :: Int -> Int -> [[Int]] -> [Int]
getDownDiagonal x y bd = ((rev(getDiagonal x y dec dec bd)) ++ (getDiagonal (x+1) (y+1) (+1) (+1) bd))

--gets diagonal going from top right to bottom left
getUpDiagonal :: Int -> Int -> [[Int]] -> [Int]
getUpDiagonal x y bd = (rev(getDiagonal x y (+1) dec bd) ++ (getDiagonal (x-1) (y+1) dec (+1) bd))

--gets diagonal based on which functions are passed to fx or fy
--i.e. fx can be +1 and fy can be -1
getDiagonal :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [[Int]] -> [Int]
getDiagonal x y fx fy bd
  | x == 0 || y == 0 || x > (length bd) || y > (length bd) = []
  | otherwise = (getCol y (row x bd)) : (getDiagonal (fx x) (fy y) fx fy bd) 

detectWin p l = detectWinH p 0 l

--detects if x is in given 1d array 5 times in a row
detectWinH :: Int -> Int -> [Int] -> Bool
detectWinH p i l
  | i == 5 = True
  | (length l) == 0 = False
  | (head l) == p = detectWinH p (i+1) (tail l)
  | otherwise = detectWinH p 0 (tail l)

--returns true if (x, y) element in given 2d array is empty
isEmpty :: Int -> Int -> [[Int]] -> Bool
isEmpty x y bd = checkValue y (row x bd) == 0

--returns true if (x, y) element in given 2d array is not empty
isMarked :: Int -> Int -> [[Int]] -> Bool
isMarked x y bd = checkValue y (row x bd) /= 0

--returns true if in (x,y) coordinate in a 2d array is == p
isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
isMarkedBy x y bd p = checkValue y (row x bd) == p

--returns value in given (x y) coordinate in 2d array
marker :: Int -> Int -> [[Int]] -> Int
marker x y bd = checkValue y (row x bd)

--takes in 1d array and gets value at index y 
checkValue :: Int -> [Int] -> Int
checkValue y l
  | y == 1 = (head l)
  | otherwise = checkValue (y-1) (tail l)

--checks if 2d array is full
isFull :: [[Int]] -> Bool
isFull bd
  | (length bd) == 0 = True
  | isFullH (head bd) = isFull (tail bd)
  | otherwise = False

--helper for isFull, checks a single 1d array to see if there's no zeros
isFullH :: [Int] -> Bool
isFullH l
  | (length l) == 0 = True
  | (head l) == 0 = False
  | otherwise = isFullH(tail l)

--marks )x, y) element in 2d array with given p value
mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
mark x y bd p
  | (length bd) == 0 = []
  | x == 1 = markCol y p (head bd) : mark (x-1) y (tail bd) p
  | otherwise = (head bd) : mark (x-1) y (tail bd) p

--Marks given 1d array l at index i with value p
markCol :: Int -> Int -> [Int] -> [Int]
markCol i p l
  | (length l) == 0 = []
  | i == 1 = p : markCol(i - 1) p (tail l)
  | otherwise = (head l) : markCol (i-1) p (tail l)

--creates first header for omok board representation in CLI
createHeader :: Int -> Int -> String
createHeader i n
   | i == 1 = " x 1" ++  createHeader (i+1) n
   | i > n = "\n" ++ createHeaderH 1 n
   | otherwise = "  " ++ (show(mod i 10)) ++ createHeader (i+1) n

--creates second header for omok board representation in CLI
createHeaderH :: Int -> Int -> String
createHeaderH i n
   | i == 1 = "y  " ++ createHeaderH (i+1) n
   | i > n = "-\n"
   | otherwise = "---" ++ createHeaderH (i+1) n

--creates string for CLI representation of board
boardToStr :: [[Int]] -> (Int -> String) -> String
boardToStr bd playerToChar = createHeader 1 (length bd) ++ createBoardStr bd playerToChar 1

--creates row and info for CLI

createBoardStr :: [[Int]] -> (Int -> String) -> Int -> String
createBoardStr bd playerToChar i
  | (length bd) == 0 = ""
  | otherwise = (show (mod i 10)) ++ "|" ++ createRowStr (head bd) playerToChar ++ createBoardStr (tail bd) playerToChar (i+1)

--turns row of ints into string with '.', 'O', 'X'
createRowStr :: [Int] -> (Int -> String) -> String
createRowStr l playerToChar
  | (length l) == 0 = "\n"
  | otherwise = (playerToChar (head l)) ++ createRowStr (tail l) playerToChar

--gives integer representation of player
mkPlayer :: Int
mkPlayer = 1

--gives integer representation of ppponent
mkOpponent :: Int
mkOpponent = 2

rev :: [Int] -> [Int]
rev l
  | (length l) == 0 = []
  | otherwise = rev (tail l) ++ [(head l)]

--decrements given int by 1
dec :: Int -> Int
dec x = x - 1

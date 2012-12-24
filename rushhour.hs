-- Rush Hour Puzzle Solver
-- by Cate Nestor & Nick Zhu
-- University of British Colbumia
-- CPSC 312 Term 1 Winter 2012

-- test boards live here:

testBoard1 = ["--B---","--B---","XXB---","--AA--","------","------"]
beginnerTestBoard = ["GG---Y","P--B-Y","PXXB-Y","P--B--","O---LL","O-TTT-"]
intTestBoard = ["YGGP--","Y--P--","YXXP--","--OBBB","--O--D","--TTTD"]
advTestBoard = ["GGOY--","P-OY--","PXXY--","PBBB--","------","---TTT"]
expertTestBoard = ["GG-YYY","---OLL","-XXO-P","--BDDP","EEB--P","--BTTT"]


-- rush_hour is the initial input function

rush_hour :: [String] -> IO ()
rush_hour start = putStr ((formatBoardList (reverse (statesearch [start] []))) ++ "\n")


-- functions for formatting the output into a nicer, readable list of boards

isSolved :: [String] -> Bool
isSolved board = ('X' == ((board !! 2) !! 5))

formatBoard :: [String] -> String
formatBoard board = foldl stringify "" board

formatBoardList :: [[String]] -> String
formatBoardList list = foldl stringify2 "" (map formatBoard list)

stringify :: String -> String -> String
stringify a b = a ++ "\n" ++ b

stringify2 :: String -> String -> String
stringify2 a b = a ++ "\n\n" ++ b


-- state space search function modified from PegPuzzle code

statesearch :: [[String]] -> [[String]] -> [[String]]
statesearch unexplored path
 | null unexplored          	 	= []
 | isSolved (head unexplored)  		= (head unexplored):path
 | (elem (head unexplored) path)	= statesearch (tail unexplored) path
 | (not (null result))          	= result
 | otherwise                    	= statesearch (tail unexplored) path
 where result = statesearch (generateNewStates (head unexplored)) ((head unexplored):path)


-- functions to generate a list of new states

generateNewStates :: [String] -> [[String]]
generateNewStates board = generateMoves board (generateCarTuples board)

generateMoves :: [String] -> [(Char, Int, Int)] -> [[String]]
generateMoves board cars
 | null cars			= []
 | (second (head cars)) == 1	= (generateHorizontalMoves board (head cars))++(generateMoves board (tail cars))
 | otherwise			= (generateVerticalMoves board (head cars))++(generateMoves board (tail cars))

generateHorizontalMoves :: [String] -> (Char, Int, Int) -> [[String]]
generateHorizontalMoves board car
 | moveForwardPossible (getRow board (third car)) (first car) && moveBackPossible (getRow board (third car)) (first car) 	= (moveRight board car):(moveLeft board car):[]
 | moveForwardPossible (getRow board (third car)) (first car)				= (moveRight board car):[]
 | moveBackPossible (getRow board (third car)) (first car)				= (moveLeft board car):[]
 | otherwise 										= []

generateVerticalMoves :: [String] -> (Char, Int, Int) -> [[String]]
generateVerticalMoves board car
 | moveForwardPossible (getColumn board (third car)) (first car) && moveBackPossible (getColumn board (third car)) (first car) 	= (moveDown board car):(moveUp board car):[]
 | moveForwardPossible (getColumn board (third car)) (first car)			= (moveDown board car):[]
 | moveBackPossible (getColumn board (third car)) (first car)				= (moveUp board car):[]
 | otherwise 										= board:[]



-- to help in generating new states, a list of cars is generated
-- cars are represented by tuples
-- a car tuple is (Char - letter of car, Int - alignment, Int - axis)
-- alignment can be either 0 for vertical or 1 for horizontal
-- axis is 0 (for top or left column/row) to 5 (for bottom or right column/row)


generateCarTuples :: [String] -> [(Char, Int, Int)]
generateCarTuples board = generateCarTuples' board [] (listCars board)

listCars :: [String] -> [Char]
listCars board
 | null board		= []
 | otherwise		= unique ((listCars' (head board)) ++ (listCars (tail board)))

listCars' :: [Char] -> [Char]
listCars' boardRow
 | null boardRow			= []
 | (head boardRow) == '-'		= listCars' (tail boardRow)
 | otherwise				= ((head boardRow):(listCars' (tail boardRow)))

unique :: [Char] -> [Char]
unique [] = []
unique (x:xs)
 | elem x xs	= unique xs
 | otherwise	= x:(unique xs)

generateCarTuples' :: [String] -> [(Char, Int, Int)] -> [Char] -> [(Char, Int, Int)]
generateCarTuples' board list cars
 | null cars	= list
 | otherwise	= (head cars, isHorizontal board (head cars), getAxisId board (head cars) (isHorizontal board (head cars))):(generateCarTuples' board list (tail cars))

getAxisId :: [String] -> Char -> Int -> Int
getAxisId board char alignment
 | alignment == 0	= getColumnId board char 5
 | otherwise		= getRowId board char

getColumnId :: [String] -> Char -> Int -> Int
getColumnId board char n
 | elem char (getColumn board n)	= n
 | otherwise				= getColumnId board char (n-1)

getRowId :: [String] -> Char -> Int
getRowId board char
 | elem char (head board)	= 0
 | otherwise			= 1 + (getRowId (tail board) char)

isHorizontal :: [String] -> Char -> Int
isHorizontal board car
 | elem car (head board) && (countElement (head board) car) == 1	= 0
 | elem car (head board) && (countElement (head board) car) /= 1	= 1
 | otherwise								= isHorizontal (tail board) car

countElement :: String -> Char -> Int
countElement string char
 | null string			= 0
 | (head string) == char	= 1 + (countElement (tail string) char)
 | otherwise			= countElement (tail string) char


-- basic functions to handle tuples of length three

first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x


-- getter and setter functions for generating new states
-- adjusting only the contents of the axis which the car to be moved is on increases the level of abstraction because it allows columns and rows to be handled in a similar way by other functions

getColumn :: [String] -> Int -> String
getColumn board n
 | null board	= []
 | otherwise	= ((head board) !! n):(getColumn (tail board) n)

getRow :: [String] -> Int -> String
getRow board n
 | null board	= []
 | otherwise	= board !! n

setColumn :: [String] -> Int -> String -> [String]
setColumn board n string
 | null string	= []
 | otherwise	= (setElement (head board) n (head string)):(setColumn (tail board) n (tail string))

setElement :: String -> Int -> Char -> String
setElement string n char
 | n == 0	= char:(tail string)
 | otherwise	= (head string):(setElement (tail string) (n-1) char)

setRow :: [String] -> Int -> String -> [String]
setRow board row content
 | row == 0	= content:(tail board)
 | otherwise    = (head board):(setRow (tail board) (row-1) content)


-- functions to move a particular car on the board

moveRight :: [String] -> (Char, Int, Int) -> [String]
moveLeft :: [String] -> (Char, Int, Int) -> [String]
moveDown :: [String] -> (Char, Int, Int) -> [String]
moveUp :: [String] -> (Char, Int, Int) -> [String]

moveRight board car = setRow board (third car) (forwardOnAxis (getRow board (third car)) (first car))
moveLeft board car = setRow board (third car) (backOnAxis (getRow board (third car)) (first car))
moveDown board car = setColumn board (third car) (forwardOnAxis (getColumn board (third car)) (first car))
moveUp board car = setColumn board (third car) (backOnAxis (getColumn board (third car)) (first car))


-- functions to test if moving a particular car is possible

moveForwardPossible :: String -> Char -> Bool
moveForwardPossible axiscontent letter
 | null (tail axiscontent)							= False
 | (head axiscontent == letter) && (head (tail axiscontent) == '-')		= True
 | otherwise									= moveForwardPossible (tail axiscontent) letter

moveBackPossible :: String -> Char -> Bool
moveBackPossible axiscontent letter = moveForwardPossible (reverse axiscontent) letter


-- functions to move a particular car on a specific axis

forwardOnAxis :: String -> Char -> String
forwardOnAxis axiscontent letter = reverse (backOnAxis (reverse axiscontent) letter)

backOnAxis :: String -> Char -> String
backOnAxis axiscontent letter
 | (head axiscontent /= letter) && null (tail axiscontent)			= (head axiscontent):[]
 | (head axiscontent == letter) && null (tail axiscontent)			= '-':[]
 | (head axiscontent /= letter) && (head (tail axiscontent) == letter)		= letter:(backOnAxis (tail axiscontent) letter)
 | (head axiscontent == letter) && (head (tail axiscontent) /= letter) 	= '-':(backOnAxis (tail axiscontent) letter)
 | otherwise									= (head axiscontent):(backOnAxis (tail axiscontent) letter)
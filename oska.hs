-- Oska Move Selector
-- by Cate Nestor & Nick Zhu
-- University of British Columbia
-- CPSC 312 Term 1 Winter 2012


import Data.Maybe


-- GENERAL SYNTAX INFORMATION
-- a square on the board is represented by a coordinate tuple:
-- (int - distance from left of board, int - distance from white homerow)
-- both parts of tuple are zero indexed

-- when colour is passed as a parameter next to a board or list of boards 
-- it should always represent the NEXT player who will move on those boards


-- TEST OSKA BOARDS

n4teststart	= ["wwww","---","--","---","bbbb"]
n4test2 	= ["ww--","-wb","-w","---","b---"]
n4test3		= ["----","-w-","b-","-wb","w---"]
n4testbwon 	= ["b--w","w--","--","w--","---w"]
n4testbwon2 = ["----","-b-","--","b--","----"]
n4testwwon 	= ["-b--","--b","-b","---","w---"]
n4testempty	= ["----","---","--","---","----"]
n5teststart	= ["wwwww","----","---","--","---","----","bbbbb"]
n5test2 	= ["w---w","----","-w-","--","bb-","-w--","b----"]




-- TOP LEVEL FUNCTION

oska :: [String] -> Char -> Int -> [String]
oska board colour depth
 | depth == 0				= ["ERROR: A move cannot be generated without looking at least one move ahead."]
 | otherwise				= pickBestmove (generateMoves board colour) (opp colour) (depth - 1)




-- MOVE GENERATION

generateMoves :: [String] -> Char -> [[String]]
generateMoves board colour = generateMoves' board colour (generatePieceArrays board)

generateMoves' :: [String] -> Char -> [[(Int, Int)]] -> [[String]]
generateMoves' board colour pieces
 | colour == 'w'	= generateWhiteMoves board (head pieces)
 | otherwise		= generateBlackMoves board (head (tail pieces))

generateWhiteMoves :: [String] -> [(Int, Int)] -> [[String]]
generateWhiteMoves board pieces
 | null pieces 	= []
 | otherwise	= (generateWhitePieceMoves board (head pieces)) ++ (generateWhiteMoves board (tail pieces))

generateWhitePieceMoves :: [String] -> (Int, Int) -> [[String]]
generateWhitePieceMoves board piece = removeEmptyArray [whiteStepLeft board piece, whiteStepRight board piece,
	whiteJumpLeft board piece, whiteJumpRight board piece]

-- Empty arrays are returned when the generation creates an invalid board. this function removes them
removeEmptyArray :: [[String]] -> [[String]]
removeEmptyArray arrays
 | null arrays 	= []
 | null (head arrays) = removeEmptyArray (tail arrays)
 | otherwise	= (head arrays):(removeEmptyArray (tail arrays))

-- Possible moves for each white piece
whiteStepLeft :: [String] -> (Int, Int) -> [String]
whiteStepLeft board (x, y)
 | getSquare board (x+(calculateWhiteXLeftCoordinate board x y 1), y+1) == '-'	= move (move board x y '-') (x+(calculateWhiteXLeftCoordinate board x y 1)) (y+1) 'w'
 | otherwise 																	= []

whiteStepRight :: [String] -> (Int, Int) -> [String]
whiteStepRight board (x, y)
 | getSquare board (x+(calculateWhiteXRightCoordinate board x y 1), y+1) == '-'	= move (move board x y '-') (x+(calculateWhiteXRightCoordinate board x y 1)) (y+1) 'w'
 | otherwise 							= []

whiteJumpLeft :: [String] -> (Int, Int) -> [String]
whiteJumpLeft board (x, y)
 | (getSquare board (x+(calculateWhiteXLeftCoordinate board x y 2), y+2) == '-') && (getSquare board (x+(calculateWhiteXLeftCoordinate board x y 1), y+1) == 'b')	= move (move (move board x y '-') (x+(calculateWhiteXLeftCoordinate board x y 1)) (y+1) '-') (x+(calculateWhiteXLeftCoordinate board x y 2)) (y+2) 'w'
 | otherwise 																= []

whiteJumpRight :: [String] -> (Int, Int) -> [String]
whiteJumpRight board (x, y)
 | (getSquare board (x+(calculateWhiteXRightCoordinate board x y 2), y+2) == '-') && (getSquare board (x+(calculateWhiteXRightCoordinate board x y 1), y+1) == 'b')	= move (move (move board x y '-') (x+(calculateWhiteXRightCoordinate board x y 1)) (y+1) '-') (x+(calculateWhiteXRightCoordinate board x y 2)) (y+2) 'w'
 | otherwise 																	= []

-- Calculates the x offset for white pieces
calculateWhiteXLeftCoordinate :: [String] -> Int -> Int -> Int -> Int
calculateWhiteXLeftCoordinate board x y ydiff
 | ydiff == 0				= 0
 | y < (getN board - 2)		= -1 + (calculateWhiteXLeftCoordinate board (x-1) (y+1) (ydiff-1))
 | otherwise				= calculateWhiteXLeftCoordinate board x (y+1) (ydiff-1)

calculateWhiteXRightCoordinate :: [String] -> Int -> Int -> Int -> Int
calculateWhiteXRightCoordinate board x y ydiff
 | ydiff == 0				= 0
 | y < (getN board - 2)		= calculateWhiteXRightCoordinate board x (y+1) (ydiff-1)
 | otherwise				= 1 + calculateWhiteXRightCoordinate board (x+1) (y+1) (ydiff-1)

generateBlackMoves :: [String] -> [(Int, Int)] -> [[String]]
generateBlackMoves board pieces
 | null pieces 	= []
 | otherwise	= (generateBlackPieceMoves board (head pieces)) ++ (generateBlackMoves board (tail pieces))

generateBlackPieceMoves :: [String] -> (Int, Int) -> [[String]]
generateBlackPieceMoves board piece = removeEmptyArray [blackStepLeft board piece, blackStepRight board piece,
	blackJumpLeft board piece, blackJumpRight board piece]

-- Possible moves for each black piece
blackStepLeft :: [String] -> (Int, Int) -> [String]
blackStepLeft board (x, y)
 | getSquare board (x+(calculateBlackXLeftCoordinate board x y 1), y-1) == '-'	= move (move board x y '-') (x+(calculateBlackXLeftCoordinate board x y 1)) (y-1) 'b'
 | otherwise 																	= []

blackStepRight :: [String] -> (Int, Int) -> [String]
blackStepRight board (x, y)
 | getSquare board (x+(calculateBlackXRightCoordinate board x y 1), y-1) == '-'	= move (move board x y '-') (x+(calculateBlackXRightCoordinate board x y 1)) (y-1) 'b'
 | otherwise 							= []

blackJumpLeft :: [String] -> (Int, Int) -> [String]
blackJumpLeft board (x, y)
 | (getSquare board (x+(calculateBlackXLeftCoordinate board x y 2), y-2) == '-') && (getSquare board (x+(calculateBlackXLeftCoordinate board x y 1), y-1) == 'w')	= move (move (move board x y '-') (x+(calculateBlackXLeftCoordinate board x y 1)) (y-1) '-') (x+(calculateBlackXLeftCoordinate board x y 2)) (y-2) 'b'
 | otherwise 																= []

blackJumpRight :: [String] -> (Int, Int) -> [String]
blackJumpRight board (x, y)
 | (getSquare board (x+(calculateBlackXRightCoordinate board x y 2), y-2) == '-') && (getSquare board (x+(calculateBlackXRightCoordinate board x y 1), y-1) == 'w')	= move (move (move board x y '-') (x+(calculateBlackXRightCoordinate board x y 1)) (y-1) '-') (x+(calculateBlackXRightCoordinate board x y 2)) (y-2) 'b'
 | otherwise 												= []

-- Finds the x offset of moving a black piece
calculateBlackXLeftCoordinate :: [String] -> Int -> Int -> Int -> Int
calculateBlackXLeftCoordinate board x y ydiff
 | ydiff == 0				= 0
 | y > (getN board - 2)		= -1 + (calculateBlackXLeftCoordinate board (x-1) (y-1) (ydiff-1))
 | otherwise				= calculateBlackXLeftCoordinate board x (y-1) (ydiff-1)

calculateBlackXRightCoordinate :: [String] -> Int -> Int -> Int -> Int
calculateBlackXRightCoordinate board x y ydiff
 | ydiff == 0				= 0	
 | y > (getN board - 2)		= calculateBlackXRightCoordinate board x (y-1) (ydiff-1)
 | otherwise				= 1 +(calculateBlackXRightCoordinate board (x+1) (y-1) (ydiff-1))

-- Replace the (x,y)th element on the board with newElement

-- move finds the row
move :: [String] -> Int -> Int -> Char -> [String]
move board x y newElement
 | y == 0		= (replaceRowElement (head board) x newElement):(tail board)
 | otherwise 	= (head board):(move (tail board) x (y-1) newElement)

-- replaceRowElement finds the column and replaces the element
replaceRowElement :: String -> Int -> Char -> String
replaceRowElement boardRow x newElement
 | x == 0		= newElement:(tail boardRow)
 | otherwise	= (head boardRow):(replaceRowElement (tail boardRow) (x-1) newElement)




-- SOME FUNCTIONS FOR FINDING THINGS OUT ABOUT THE BOARD

-- find the character at a coordinate
-- if the square is out of the bounds of the board, the value 'X' will be returned
getSquare :: [String] -> (Int, Int) -> Char
getSquare board (x,y)
 | x < 0										= 'X'
 | y < 0										= 'X'
 | y >= ((2 * (getN board)) - 4)				= 'X'
 | x >= (expectedRowLength (getN board) y)		= 'X'
 | otherwise									= ((board !! (y)) !! (x))

-- gets the size of the board
-- the number of rows in the board = (2n - 3)

getN :: [String] -> Int
getN board = length (head board)

-- row length = 2 + distance from middle row
-- this is not zero indexed for now

expectedRowLength :: Int -> Int -> Int
expectedRowLength n row = 2 + (abs (row - (n - 2)))


-- to evaluate the board, a pair of arrays is created, each containing the coordinates of the pieces
-- the first array represents the white pieces
-- while the second represents the black
-- (in a valid board, there should not be more than n items in each array)

generatePieceArrays :: [String] -> [[(Int, Int)]]
generatePieceArrays board = scanAllRows board 0 [[],[]]

scanAllRows :: [String] -> Int -> [[(Int, Int)]] -> [[(Int, Int)]]
scanAllRows board y pieces
 | null board					= pieces
 | otherwise 					= scanAllRows (tail board) (y + 1) (scanRow y (head board) 0 pieces)


scanRow :: Int -> String -> Int -> [[(Int, Int)]] -> [[(Int, Int)]]
scanRow y rowcontents x pieces
 | null rowcontents				= pieces
 | (head rowcontents) == 'w'	= scanRow y (tail rowcontents) (x + 1) (((x,y):(head pieces)):(tail pieces))
 | (head rowcontents) == 'b'	= scanRow y (tail rowcontents) (x + 1) ((head pieces):(((x,y):(head (tail pieces))):(tail (tail pieces))))
 | otherwise 					= scanRow y (tail rowcontents) (x + 1) pieces



-- BOARD EVALUATOR

-- the score is negative if good for black, and positive if good for white, no matter the player
-- thus, the implementation of the minimax must take the player into account
-- decided to use non-tail recursion for some score generation/getting n because there will be at most n recursions 
-- in those functions so there will never be too much on the stack

getScore :: [String] -> Int
getScore board = getScore' (generatePieceArrays board) (getN board)

getScore' :: [[(Int, Int)]] -> Int -> Int 
getScore' pieces n
 | (isWonForWhite pieces n)		= (n * ((2 * n) - 4))
 | (isWonForBlack pieces)		= ((-1* n) * ((2 * n) - 4))
 | otherwise					= ((getScoreW (head pieces)) - (getScoreB (last pieces) ((2 * n) - 4)))

isWonForWhite :: [[(Int, Int)]] -> Int -> Bool
isWonForWhite pieces n
 | null (tail pieces)			= True
 | null (head pieces)			= False
 | otherwise					= isWonForWhite' (head pieces) ((2 * n) - 4)

isWonForWhite' :: [(Int, Int)] -> Int -> Bool
isWonForWhite' wpieces goal
 | null wpieces						= True
 | (snd (head wpieces) /= goal)		= False
 | otherwise 						= isWonForWhite' (tail wpieces) goal

isWonForBlack :: [[(Int, Int)]] -> Bool
isWonForBlack pieces
 | null (head pieces)			= True
 | null (tail pieces)			= False
 | otherwise					= isWonForBlack' (last pieces)

isWonForBlack' :: [(Int, Int)] -> Bool
isWonForBlack' bpieces
 | null bpieces						= True
 | (snd (head bpieces) /= 0)		= False
 | otherwise 						= isWonForBlack' (tail bpieces)

getScoreW :: [(Int,Int)] -> Int
getScoreW wpieces
 | null wpieces						= 0
 | otherwise						= ((snd (head wpieces)) + (getScoreW (tail wpieces)))

getScoreB :: [(Int,Int)] -> Int -> Int
getScoreB bpieces brow
 | null bpieces						= 0
 | otherwise						= ((brow - (snd (head bpieces))) + (getScoreB (tail bpieces) brow))



-- MINIMAX SEARCH

-- there is a seperate function for the top level because this is the only level at which the scores must be associated with a particular board
-- thus tuples are created instead of just integers in order to keep track of the move they represent 
pickBestmove :: [[String]] -> Char -> Int -> [String]
pickBestmove moves colour depth
 | null moves 				= ["ERROR: There are no possible moves for this player."]
 | null (tail moves)		= (head moves)
 | otherwise 				= (getTupleMinimax (catMaybeTuples (getMoveTuples moves colour depth [])) colour)

getMoveTuples :: [[String]] -> Char -> Int -> [([String], Maybe Int)] -> [([String], Maybe Int)]
getMoveTuples moves colour depth tuples
 | null moves 				= tuples
 | otherwise				= (getMoveTuples (tail moves) colour depth ((getMoveTuple (head moves) colour depth):tuples))

getMoveTuple :: [String] -> Char -> Int -> ([String], Maybe Int)
getMoveTuple board colour depth = (board, (minimaxSearch board colour depth))

-- returns integer score for a board at a certain depth
minimaxSearch :: [String] -> Char -> Int -> Maybe Int
minimaxSearch board colour depth
 | depth == 0 												= Just (getScore board)
 | otherwise												= getMinimax (catMaybes (minimaxMap (generateMoves board colour) (opp colour) (depth - 1))) colour

-- the minimax algorithm is depth-first, as it fully calculates the score of each board in a set of moves before moving on
-- this function looks through each board in a set of moves in order
minimaxMap :: [[String]] -> Char -> Int -> [Maybe Int]
minimaxMap moves colour depth
 | null moves 										= []
 | otherwise 										= (minimaxSearch (head moves) colour depth):(minimaxMap (tail moves) colour depth)


getMinimax :: [Int] -> Char -> Maybe Int
getMinimax scores colour
 | null scores	  = Nothing
 | colour == 'b'  = Just (getMax scores)
 | otherwise 	  = Just (getMin scores)

getMin :: [Int] -> Int
getMin scores
 | null (tail scores)							 	= head scores
 | (head scores) > (head (tail scores))				= getMin (tail scores)
 | otherwise										= getMin ((head scores):(tail (tail scores)))

getMax :: [Int] -> Int
getMax scores
 | null (tail scores)							 	= head scores
 | (head scores) < (head (tail scores))				= getMax (tail scores)
 | otherwise										= getMax ((head scores):(tail (tail scores)))

-- these are similar functions to the above, but for tuples instead

getTupleMinimax :: [([String],Int)] -> Char -> [String]
getTupleMinimax movetuples colour
 | null movetuples  = ["Error: There are no further moves at this level. Please try a lower depth."]
 | colour == 'b'  	= getTupleMax movetuples
 | otherwise 	  	= getTupleMin movetuples

getTupleMin :: [([String],Int)] -> [String]
getTupleMin movetuples
 | null (tail movetuples)								 	= fst (head movetuples)
 | snd (head movetuples) > snd (head (tail movetuples))		= getTupleMin (tail movetuples)
 | otherwise												= getTupleMin ((head movetuples):(tail (tail movetuples)))

getTupleMax :: [([String],Int)] -> [String]
getTupleMax movetuples
 | null (tail movetuples)							 		= fst (head movetuples)
 | snd (head movetuples) < snd (head (tail movetuples))		= getTupleMax (tail movetuples)
 | otherwise												= getTupleMax ((head movetuples):(tail (tail movetuples)))


-- EXTRA HELPER FUNCTIONS

append :: [a] -> [a] -> [a]
append list1 list2
 | null list1	= list2
 | otherwise	= (head list1):(append (tail list1) list2)

opp :: Char -> Char
opp colour
 | colour == 'w' 	= 'b'
 | otherwise 		= 'w'

catMaybeTuples :: [([String], Maybe Int)] -> [([String], Int)]
catMaybeTuples movetuples
 | (null movetuples) 				= []
 | isJust(snd (head movetuples)) 	= ((fst (head movetuples)),(fromJust (snd (head movetuples)))):(catMaybeTuples (tail movetuples))
 | otherwise						= (catMaybeTuples (tail movetuples))



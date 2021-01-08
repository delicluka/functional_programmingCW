module Main where

import Data.Char
import Data.Ord

-- define useful datatypes

data Player = X | O
data PlayerAction = Quit | Play Move
data Cell = Emp | Fill Player
type Board = [[Cell]]
type Move = Int
data Score = PlusInf | MinusInf | Fin Int
type AIChoice = (Score, Move)
data Strategy = Max | Min
data GameOutcome = Win Player | Draw | Going

-- equality and inequality instances, useful for comparison

instance Eq Score where
  (==) PlusInf PlusInf = True
  (==) MinusInf MinusInf = True
  (==) (Fin x) (Fin y) = x == y
  (==) _ _ = False

instance Eq Cell where
  (==) Emp Emp = True
  (==) (Fill p) (Fill p') = p == p'
  (==) _ _ = False

instance Eq Player where
  (==) O O = True
  (==) X X = True
  (==) _ _ = False

instance Ord Score where
  (<=) MinusInf _ = True
  (<=) _ MinusInf = False
  (<=) _ PlusInf = True
  (<=) PlusInf _ = False
  (<=) (Fin x) (Fin y) = x <= y

-- four directions (sufficient) for checking sequences of Xs and Os
directions :: [(Int, Int)]
directions = [(0, 1), (1, -1), (1, 0), (1, 1)]

-- adding numbers, lifted to pairs of Ints
addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x, y) (dx, dy) = (x + dx, y + dy)

-- helpers for handling strategy and player logic

otherStrategy :: Strategy -> Strategy
otherStrategy Max = Min
otherStrategy Min = Max

strategyFunction :: Strategy -> [AIChoice] -> AIChoice
strategyFunction Max = maximum
strategyFunction Min = minimum

otherPlayer :: Player -> Player
otherPlayer O = X
otherPlayer X = O

-- Scoring a board with a particular player in mind.
-- The score is evaluated separately for each player, and each of the scores
-- has two components:
-- * extend2: how many 2-sequences of a certain symbol there are which can be
--            extended to 3-sequences
-- * holes4: how many sequences of 3 of a certain symbol and one empty cells
--           there are
computeScore :: Board -> Player -> Score
computeScore board player =
  let insideBoard (x, y) = x >= 0 && x <= 6 && y >= 0 && y <= 5
      getCell (x, y) = board !! x !! y
      extend2 player' =
        let validTriple cell dir =
              all insideBoard [cell, addTuple cell dir,
                               addTuple (addTuple cell dir) dir] &&
              ((getCell cell == Fill player' &&
                getCell (addTuple cell dir) == Fill player' &&
                getCell (addTuple (addTuple cell dir) dir) == Emp) ||
               (getCell cell == Emp &&
                getCell (addTuple cell dir) == Fill player' &&
                getCell (addTuple (addTuple cell dir) dir) == Fill player'))
        in length [() | x <- [0 .. 6], y <- [0 .. 5], dir <- directions,
                        validTriple (x, y) dir]
      holes4 player' =
        let sequence cell dir =
              [cell, addTuple cell dir, addTuple (addTuple cell dir) dir,
               addTuple (addTuple (addTuple cell dir) dir) dir]
            validSequence seq =
              all insideBoard seq &&
              all (/=Fill (otherPlayer player')) (map getCell seq) &&
              length [() | t <- seq, getCell t == Fill player'] == 3
        in length [() | x <- [0 .. 6], y <- [0 .. 5], dir <- directions,
                        validSequence (sequence (x, y) dir)]
      playerScore player' = extend2 player' + 5 * holes4 player'
  in Fin (playerScore player - playerScore (otherPlayer player))

-- An implementation of the Minimax algorithm.
playAI :: Board -> Player -> Player -> Strategy -> Int -> AIChoice
playAI board aiPlayer player strategy ply =
  case endBoard board of
    Win player' -> if aiPlayer == player' then (PlusInf, 0) else (MinusInf, 0)
    Draw -> (Fin 0, 0)
    Going ->
      if ply == 0
        then (computeScore board aiPlayer, 0)
        else
          let validMoves = getValidMoves board
              nextMoveScore move =
                (fst (playAI (updateBoard board move player) aiPlayer
                     (otherPlayer player) (otherStrategy strategy) (ply - 1)),
                 move)
          in strategyFunction strategy (map nextMoveScore validMoves)

-- empty board (initial state of the game)
emptyBoard :: Board
emptyBoard = map (\_ -> map (\_ -> Emp) [0 .. 5]) [0 .. 6]

-- rotating the board 90 degrees
boardTranspose :: Board -> Board
boardTranspose board =
  reverse (map (\i -> map (\bc -> bc !! i) board) [0 .. 5])

-- helper function on lists, equivalent to python enumerate
enumerate :: [a] -> [(Int, a)]
enumerate l = zip [0..] l

-- return the list of valid moves a player can play on a board
getValidMoves :: Board -> [Move]
getValidMoves board =
  let columnValid [] = False
      columnValid (Emp : _) = True
      columnValid (_ : c) = columnValid c
      validCols = filter (\(idx, c) -> columnValid c) (enumerate board)
  in map fst validCols

-- return a board after a move by a player
updateBoard :: Board -> Move -> Player -> Board
updateBoard board move player =
  let updateColumn (Emp : xs) = (Fill player) : xs
      updateColumn (x : xs) = x : (updateColumn xs)
      updateBoard' [] _ = []
      updateBoard' (c : cs) n = if n == move then (updateColumn c) : cs
                                             else c : (updateBoard' cs (n + 1))
  in updateBoard' board 0

-- a set of functions checking whether the game is over, and what its outcome is

-- are there 4 consecutive symbols (vertical, arbitrary board size)
endConsecutive :: Board -> [Int] -> GameOutcome
endConsecutive board range = 
  let checkCons _ (Fill player) 0 = Win player
      checkCons _ Emp 0 = Draw
      checkCons [] _ _ = Draw
      checkCons (Emp : x) _ _ = checkCons x Emp 4
      checkCons (Fill O : x) (Fill O) n = checkCons x (Fill O) (n - 1)
      checkCons (Fill O : x) _ n = checkCons x (Fill O) 3
      checkCons (Fill X : x) (Fill O) n = checkCons x (Fill X) 3
      checkCons (Fill X : x) _ n = checkCons x (Fill X) (n - 1)
      firstOutcome [] = Draw
      firstOutcome (Draw : outcomes) = firstOutcome outcomes
      firstOutcome ((Win player) : outcomes) = Win player
  in firstOutcome [checkCons (board !! i) Emp 4 | i <- range]

-- checking whether there are 4 consecutive identical symbols, vertically
endVertical :: Board -> GameOutcome
endVertical board = endConsecutive board [0 .. 6]

-- checking whether there are 4 consecutive identical symbols, horizontally
endHorizontal :: Board -> GameOutcome
endHorizontal board = endConsecutive (boardTranspose board) [0 .. 5]

-- checking whether there are 4 consecutive identical symbols, diagonally
endDiagonal :: Board -> GameOutcome
endDiagonal board =
  let checkCons _ _ (Fill player) 0 = Win player
      checkCons _ _ Emp 0 = Draw
      checkCons (i, j) di sym n =
        if i < 0 || i > 6 || j < 0 || j > 5
          then Draw
          else case (sym, board !! i !! j) of
                 (Fill O, Fill O) -> checkCons (i + di, j + 1) di (Fill O) (n - 1)
                 (Fill X, Fill X) -> checkCons (i + di, j + 1) di (Fill X) (n - 1)
                 (_, cell) -> checkCons (i + di, j + 1) di cell 3
      firstOutcome [] = Draw
      firstOutcome (Draw : outcomes) = firstOutcome outcomes
      firstOutcome ((Win player) : outcomes) = Win player
  in firstOutcome [checkCons (i, j) di Emp 4 | i <- [0 .. 6], di <- [-1, 1],
                                               j <- [0 .. 2]]

-- is the board full (determining if the game is a draw)
boardFull :: Board -> Bool
boardFull board =
  let isNotEmp Emp = False
      isNotEmp _ = True
  in all (all isNotEmp) board

-- combine the previous function to determine the outcome of the game
endBoard :: Board -> GameOutcome
endBoard board =
  case endVertical board of
    Win player -> Win player
    Draw -> case endHorizontal board of
              Win player -> Win player
              Draw -> case endDiagonal board of
                        Win player -> Win player
                        Draw -> if boardFull board then Draw else Going

-- handling output (game state and messages)

charRep :: Cell -> Char
charRep Emp = '.'
charRep (Fill X) = 'X'
charRep (Fill O) = 'O'

outputBoard :: Board -> IO ()
outputBoard b =
  let tb = boardTranspose b in
    let boardRep = map (map charRep) tb in
      mapM_ putStrLn boardRep

titleCard :: [String]
titleCard = ["========================",
             "      CONNECT FOUR      ",
             "          by Luka Delic ",
             "========================",
             ""]

winMessage :: IO ()
winMessage = do
  putStrLn ""
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "You win this time, human!"

loseMessage :: IO ()
loseMessage = do
  putStrLn ""
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "Better luck next time!"

drawMessage :: IO ()
drawMessage = do
  putStrLn ""
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "Wow, a draw! Play another one?"

-- handling AI turns 
getAIInput :: Player -> Int -> Board -> IO ()
getAIInput player ply board = do
  putStrLn ""
  putStrLn ""
  outputBoard board
  putStrLn "My turn!"
  let bestMove = playAI board player player Max ply
  checkBoard (updateBoard board (snd bestMove) player)
             (getHumanInput (otherPlayer player) ply)
             loseMessage

-- get human player input
getHumanInput :: Player -> Int -> Board -> IO ()
getHumanInput player ply board = do
    let validMoves = getValidMoves board
    putStrLn ""
    putStrLn ""
    outputBoard board
    putStrLn "Your turn, human!"
    putStrLn "  q: quit"
    mapM_ (\i -> putStrLn ("  " ++ [intToDigit (i + 1)] ++
           ": add token to column " ++ [intToDigit (i + 1)])) validMoves
    action <- playerCommand validMoves
    case action of
      Quit -> loseMessage
      Play move -> checkBoard (updateBoard board move player)
                              (getAIInput (otherPlayer player) ply)
                              winMessage

-- control input of player commands, repeat if input is invalid
playerCommand :: [Move] -> IO PlayerAction
playerCommand validMoves = do
  let validChars = [intToDigit (i + 1) | i <- validMoves]
  putStr "Command: "
  getCharInput <- getChar
  if getCharInput == 'Q' || getCharInput == 'q'
    then return Quit
    else if any (getCharInput==) validChars
      then return (Play (digitToInt getCharInput - 1))
      else do
        putStrLn ""
        putStrLn "Invalid input!"
        playerCommand validMoves

-- advance game after every turn, or possibly end it
checkBoard :: Board -> (Board -> IO ()) -> IO () -> IO ()
checkBoard board nextPlay ending =
  case endBoard board of
    Win _ -> do putStrLn ""
                putStrLn ""
                outputBoard board
                ending
    Draw -> do putStrLn ""
               putStrLn ""
               outputBoard board
               drawMessage
    Going -> nextPlay board

-- input game parameters

-- input AI difficulty
inputPly :: IO Int
inputPly = do
  putStrLn ""
  putStr "Input AI difficulty (between 1 and 4): "
  charInput <- getChar
  if charInput >= '1' && charInput <= '4'
    then return (digitToInt charInput)
    else do putStrLn ""
            putStrLn "Invalid input!"
            inputPly

-- input which player plays first
inputHumanPlayer :: IO Player 
inputHumanPlayer = do
  putStrLn ""
  putStr "You play as O (first) or X (second): "
  charInput <- getChar
  case charInput of
    'x' -> return X
    'X' -> return X
    'o' -> return O
    'O' -> return O
    _ -> do putStrLn ""
            putStrLn "Invalid input!"
            inputHumanPlayer

-- initiate first turn
startGame :: Player -> Int -> IO ()
startGame O ply = getHumanInput O ply emptyBoard
startGame X ply = getAIInput O ply emptyBoard

-- entry point: initial message, input parameters and start the game
main :: IO ()
main = do
  mapM_ putStrLn titleCard
  ply <- inputPly
  humanPlayer <- inputHumanPlayer
  startGame humanPlayer ply

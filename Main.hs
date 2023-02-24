--Andres Segura
--80376343
--Dr.Cheon
--Programming Languages
--11-18-22

module Main where

import Board
import System.IO
import System.Exit
import System.Random

main = do
    putStrLn "--------------------------OMOK--------------------------"
    putStrLn "Enter 1 for random strategy or 2 to play human opponent"
    response <- getX
    run (mkBoard 15) response
    

--runs OMOK game
--prints board
--gets move from the players
run bd rs = do
    putStrLn (showBoard bd)
    putStrLn "Player turn 'X':"
    move <- readXY bd (mkPlayer)
    let bd2 = (mark (fst move) (snd move) bd (mkPlayer))
    checkWin bd2 (fst move) (snd move) (mkPlayer)
    putStrLn (showBoard bd2)
    if rs == 1 then do --random strategy
        move2 <- randomMove bd2
        putStrLn "Computer Random Move"
        let bd3 = (mark (fst move2) (snd move2) bd2 (mkOpponent))
        checkWin bd3 (fst move2) (snd move2) (mkOpponent)
        run bd3 rs
    else do
        putStrLn "Opponent turn 'O':"
        move2 <- readXY bd2 (mkOpponent)
        let bd3 = (mark (fst move2) (snd move2) bd2 (mkOpponent))
        checkWin bd3 (fst move2) (snd move2) (mkOpponent)
        run bd3 rs

--checks winner and functionality if move is a win
checkWin bd x y p = do 
    if (detectWin p (row x bd)) || (detectWin p (col y bd))
        || (detectWin p (getDownDiagonal x y bd))
        || (detectWin p (getUpDiagonal x y bd))
        then do 
            putStrLn (showBoard bd)
            if p == 1 then do
                putStrLn "---------------Player 1 - X Winning Move---------------"
            else do
                putStrLn "---------------Player 2 - O Winning Move---------------"
            putStrLn ""
            main
    else if isFull bd
        then do 
            putStrLn (showBoard bd)
            putStrLn "DRAW"
            main
    else return()

--Gets (x,y) input from user 
--checks if space is empty and if user wants to exit
readXY bd p = do
    putStrLn "Enter -1 to exit"
    putStr "Enter x coordinate: "
    x <- getX
    if x == -1 then exitProgram
    else do
        putStr "Enter y coordinate: "
        y <- getX
        putStrLn ""
        if y == -1 then exitProgram
        else if isMarked y x bd then readXYError bd p
        else return (y, x)

--handles functionality for when position is filled 
readXYError bd p = do
    putStrLn "Position already filled"
    readXY bd p 
    
--gets number from user
getX = do
       line <- getLine
       let parsed = reads line :: [(Int, String)] in
         if length parsed == 0
         then getX'
         else let (x, _) = head parsed in
           if x == -1 || (x > 0 && x < 16)
           then return x
           else getX'
       where
         getX' = do
           putStrLn "Invalid input! Try Again"
           getX

--quits program when user enter -1 for x or y move
exitProgram = do
  putStrLn "Thank you come again"
  exitWith (ExitFailure 44)

--transforms int value to a string representation
--used to transform [[int]] into a CLI representation of a board
playerToChar :: Int -> String
playerToChar 0 = " . "
playerToChar 1 = " X "
playerToChar 2 = " O "

--prints game board onto CLI
showBoard bd = (boardToStr bd playerToChar)

--gets random x y coordinate
randomMove bd = do
    x <- randomRIO(1, 15)
    y <- randomRIO(1, 15)
    if isEmpty x y bd then return (x, y)
    else randomMove bd

evens x = (mod x 2) == 0

zeros = [x | x <- [2..], evens x]
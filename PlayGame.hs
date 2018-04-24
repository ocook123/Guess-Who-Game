{-# LANGUAGE ScopedTypeVariables #-}
module PlayGame where 
    
import ReadCharacters
import Character
import Input
import System.Environment
import System.IO.Unsafe
import Control.Monad.ST.Lazy
import Data.Time.Clock
import OptimalPick

main :: IO ()
main = do
    let file = "listOfFiles.txt"
    allCharacters <- readCharacters file
    
    let player1Choices = allCharacters
    let player2Choices = allCharacters

    if length allCharacters == 0 then
        putStrLn "Error: No characters found!"
    else do
        putStrLn "Let's play!\n"
        
        gameMode <- pvpOrpvc

        --Person vs Person
        if(gameMode) then do
            randChar1Index <- getRandom (length allCharacters)
            randChar2Index <- getSecondRandom (length allCharacters) randChar1Index
            let player1Character = getCharAtIndex allCharacters randChar1Index
            let player2Character = getCharAtIndex allCharacters randChar2Index
            
            putStrLn ("Player 1's Character: \n" ++ toString player1Character ++ "\n")
            putStrLn ("Player 2's Character: \n" ++ toString player2Character ++ "\n")
            
            whoWon <- play player1Character player2Character player1Choices player2Choices gameMode
            putStrLn(whoWon)
        else do
            randChar1Index <- getRandom (length allCharacters)
            randChar2Index <- getSecondRandom (length allCharacters) randChar1Index
            let playerCharacter = getCharAtIndex allCharacters randChar1Index
            let computerCharacter = getCharAtIndex allCharacters randChar2Index

            putStrLn ("Player's Character: \n" ++ toString playerCharacter ++ "\n")

            whoWon <- play playerCharacter computerCharacter player1Choices player2Choices gameMode
            putStrLn(whoWon)


--mode: pvp = True pvc = False
play :: Character -> Character -> [Character] -> [Character] -> Bool -> IO String
play player1Character player2Character player1Choices player2Choices mode= do
    player1Choices <- turn 1 player2Character player1Choices
    printCharacters player1Choices
    if(gameIsDone player1Choices) then do
        return "Player 1 wins!"
    else do
        if(mode) then do
            player2Choices <- turn 2 player1Character player2Choices
            printCharacters player2Choices
            if(gameIsDone player2Choices) then do
                return "Player 2 wins!"
            else play player1Character player2Character player1Choices player2Choices mode
        else do
            let player2Choices = pickProperty player2Choices player1Character
            printCharacters player2Choices
            if(gameIsDone player2Choices) then do
                return "Computer wins!"
            else play player1Character player2Character player1Choices player2Choices mode

pvpOrpvc :: IO Bool
pvpOrpvc = do
    putStrLn "Select a Game Mode:"
    putStrLn "0: Player vs. Player"
    putStrLn "1: Player vs. Computer"
    mode <- getLine
    let modeVal = read mode :: Int
    if(modeVal == 0) then 
        return True
    else 
        if(modeVal == 1) then
            return False
        else pvpOrpvc

turn :: Int -> Character -> [Character] -> IO [Character]
turn player solution c = do
    putStrLn ("Player " ++ show player ++ " Remaining Characters:")
    printCharacters c

    putStrLn "Select a question or make a guess:"
    putStrLn "0: Guess the character's name"
    putStrLn "1: Ask about gender"
    putStrLn "2: Ask about department"
    putStrLn "3: Ask about country"
    putStrLn "4: Ask about phone type"
    putStrLn "5: Ask about glasses"
    putStrLn "6: Ask about cryptocurrency"

    input <- getLine
    let n = read input :: Int
    newCharacters <- handleQuestion n solution c
    return newCharacters


printCharacters ::[Character] -> IO ()
printCharacters [] = putStrLn "\n"
printCharacters (x : y) = do
    putStrLn (toString x ++ "\n")
    printCharacters y

gameIsDone :: [Character] -> Bool
gameIsDone (x : []) = True
gameIsDone _ = False

----------------------------------------------------------------------------
-- Functions used to randomly select characters at the beginning of the game
----------------------------------------------------------------------------
getRandom :: Int -> IO Int
getRandom size = do
    charSelect <- getCurrentTime
    let diff = utctDayTime charSelect
    let final = fromEnum diff
    let smaller = final `div` 10000000000
    let withMod = smaller `mod` size
    return withMod

getSecondRandom :: Int -> Int -> IO Int
getSecondRandom size avoidIndex = do
    firstTry <- getRandom size
    if(firstTry == avoidIndex) then 
        getSecondRandom size avoidIndex
    else return firstTry

getCharAtIndex :: [Character] -> Int -> Character
getCharAtIndex (x : xs) num = if (num == 0) then x
    else getCharAtIndex xs (num - 1)
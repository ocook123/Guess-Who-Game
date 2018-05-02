{-# LANGUAGE ScopedTypeVariables #-}
module PlayGame where 
    
import ReadCharacters
import Character
import Input
import Statistics
import System.Environment
import System.IO.Unsafe
import Control.Monad.ST.Lazy
import Data.Time.Clock
import OptimalPick
import Text.Read

main :: IO ()
main = do
    let file = "listOfFiles.txt"
    allCharacters <- readCharacters file
    
    endString <- beginningStart allCharacters
    putStrLn (endString)

beginningStart :: [Character] -> IO String
beginningStart allCharacters = do
    let player1Choices = allCharacters
    let player2Choices = allCharacters

    if length allCharacters == 0 then do
        putStrLn "Error: No characters found!"
        return "All done."
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

            ourStats <- readStatistics
            if(whoWon == "Player 1 wins!") then do
                let newStats = Statistics {player1pvpWins = ((player1pvpWins ourStats) + 1), player2pvpWins = (player2pvpWins ourStats), player1pvcWins = (player1pvcWins ourStats), comppvcWins = (comppvcWins ourStats)}
                printStatistics newStats gameMode
                writeStatistics newStats
            else do
                let newStats = Statistics {player1pvpWins = (player1pvpWins ourStats), player2pvpWins = ((player2pvpWins ourStats) + 1), player1pvcWins = (player1pvcWins ourStats), comppvcWins = (comppvcWins ourStats)}
                printStatistics newStats gameMode
                writeStatistics newStats


            again <- playAgain
            if(again) then beginningStart allCharacters else return "All done."
            
        --Player vs. Computer
        else do
            randChar1Index <- getRandom (length allCharacters)
            randChar2Index <- getSecondRandom (length allCharacters) randChar1Index
            let playerCharacter = getCharAtIndex allCharacters randChar1Index
            let computerCharacter = getCharAtIndex allCharacters randChar2Index

            putStrLn ("Player's Character: \n" ++ toString playerCharacter ++ "\n")

            whoWon <- play playerCharacter computerCharacter player1Choices player2Choices gameMode
            putStrLn(whoWon)
            
            --updates statistics
            ourStats <- readStatistics
            if(whoWon == "Player 1 wins!") then do
                let newStats = Statistics {player1pvpWins = (player1pvpWins ourStats), player2pvpWins = (player2pvpWins ourStats), player1pvcWins = ((player1pvcWins ourStats) + 1), comppvcWins = (comppvcWins ourStats)}
                printStatistics newStats gameMode
                writeStatistics newStats
            else do
                let newStats = Statistics {player1pvpWins = (player1pvpWins ourStats), player2pvpWins = (player2pvpWins ourStats), player1pvcWins = (player1pvcWins ourStats), comppvcWins = ((comppvcWins ourStats) + 1)}
                printStatistics newStats gameMode
                writeStatistics newStats

            again <- playAgain
            if(again) then beginningStart allCharacters else return "All done."


playAgain:: IO Bool
playAgain = do
    putStrLn("Play Again? Y or N")
    yOrn <- getLine
    if(yOrn == "Y") then do
        return True
    else do
        if(yOrn == "N") then do
            return False
        else playAgain

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
            let compChoices = pickProperty player2Choices player1Character
            putStrLn "\n\n****************************************************"
            putStrLn "The computer has the following characters:"
            putStrLn "****************************************************\n"
            printCharacters compChoices
            if(gameIsDone compChoices) then do
                return "Computer wins!"
            else play player1Character player2Character player1Choices compChoices mode

-- Allows the Player/Players to choose the game mode
pvpOrpvc :: IO Bool
pvpOrpvc = do
    putStrLn "Select a Game Mode:"
    putStrLn "0: Player vs. Player"
    putStrLn "1: Player vs. Computer"
    modeVal <- checkForInt
    if(modeVal == 0) then 
        return True
    else 
        if(modeVal == 1) then
            return False
        else pvpOrpvc

turn :: Int -> Character -> [Character] -> IO [Character]
turn player solution c = do
    putStrLn "\n\n****************************************************"
    putStrLn ("Player " ++ show player ++ "'s Turn!")
    putStrLn "****************************************************\n"

    putStrLn "Remaining Characters To Guess:"
    printCharacters c

    putStrLn "Select a question or make a guess:"
    putStrLn "0: Guess the character's name"
    putStrLn "1: Ask about gender"
    putStrLn "2: Ask about department"
    putStrLn "3: Ask about country"
    putStrLn "4: Ask about phone type"
    putStrLn "5: Ask about glasses"
    putStrLn "6: Ask about cryptocurrency"

    n <- checkForInt
    newCharacters <- handleQuestion n solution c
    return newCharacters

--Checks that the input is an integer
checkForInt :: IO Int
checkForInt = do
    input <- getLine
    case readMaybe input of
        Just x -> return x
        Nothing -> putStrLn "Please enter a valid input" >> checkForInt

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
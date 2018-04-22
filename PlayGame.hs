module PlayGame where 
    
import ReadCharacters
import System.Environment
import System.IO.Unsafe
import Control.Monad.ST.Lazy
import Character
import Data.Time.Clock

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
        
        --Person vs Person
        randChar1Index <- getRandom (length allCharacters)
        randChar2Index <- getSecondRandom (length allCharacters) randChar1Index
        let player1Character = getCharAtIndex allCharacters randChar1Index
        let player2Character = getCharAtIndex allCharacters randChar2Index
        putStrLn ("Player 1's Character: \n" ++ toString player1Character ++ "\n")
        putStrLn ("Player 2's Character: \n" ++ toString player2Character ++ "\n")
        
        player1Choices <- turn 1 player1Choices
        printCharacters player1Choices


turn :: Int -> [Character] -> IO [Character]
turn player c = do
    putStrLn ("Player " ++ show player ++ " Remaining Characters:")
    printCharacters c
    putStrLn "Select a question or make a guess:"

    -- To do: Make user select a question
    -- then call corresponding function from Character.hs
    -- Example:
    let newCharacters = removeGender c True -- Remove all males
    return newCharacters


printCharacters ::[Character] -> IO ()
printCharacters [] = putStrLn "\n"
printCharacters (x : y) = do
    putStrLn (toString x ++ "\n")
    printCharacters y

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
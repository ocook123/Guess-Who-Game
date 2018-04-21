module PlayGame where 
    
import ReadCharacters
import System.Environment
import System.IO.Unsafe
import Control.Monad.ST.Lazy
import Character

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
        let player1Character = head allCharacters -- Change!
        let player2Character = last allCharacters -- Change!
        
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


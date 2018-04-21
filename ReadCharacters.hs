module ReadCharacters where

import System.Environment
import System.IO.Unsafe
import Control.Monad.ST.Lazy
import Character

readCharacters :: String -> IO [Character]
readCharacters file = do
    contents <- readFile file
    
    if (null contents) then
        return []
    else do
        let linesOfFile = lines contents
        characterList <- readFileChar linesOfFile
<<<<<<< HEAD:ReadCharacters.hs
        return characterList
=======
        putStrLn (printCharList characterList)
>>>>>>> origin/master:readingCharacters.hs

readFileChar :: [String] -> IO [Character]
readFileChar [] = return []
readFileChar (line : restOfLines) = unsafeInterleaveIO $ do
    newChar <- readingLine line
    rest <- readFileChar restOfLines
    return (newChar : rest)
    
readingLine :: String -> IO Character
readingLine line  = do
    chara <- readFile line
    let test = (read chara) :: Character
    return test

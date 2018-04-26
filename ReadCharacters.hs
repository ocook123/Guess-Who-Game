module ReadCharacters where

import System.Environment
import System.IO.Unsafe
import Control.Monad.ST.Lazy
import Character

----------------------------------------------------------------------------
-- Functions to read in the file that contains a list of file paths and from
-- there reads in from each file named and returns a list of Characters
----------------------------------------------------------------------------
readCharacters :: String -> IO [Character]
readCharacters file = do
    contents <- readFile file
    
    if (null contents) then
        return []
    else do
        let linesOfFile = lines contents
        characterList <- readFileChar linesOfFile
        return characterList


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

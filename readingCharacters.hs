import System.Environment
import System.IO.Unsafe
import Control.Monad.State.Lazy
import Character

main :: IO ()
main = do
    let file = "listOfFiles.txt"
    contents <- readFile file
    if (null contents) then
        putStrLn "File is empty, no characters"
    else do
        let linesOfFile = lines contents
        characterList <- readFileChar linesOfFile
        putStrLn (printChar (head characterList))
        putStrLn (printChar (last characterList))

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
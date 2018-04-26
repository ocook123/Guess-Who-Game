module Statistics where


data Statistics = Statistics { player1pvpWins :: Int , player2pvpWins :: Int , player1pvcWins :: Int , comppvcWins :: Int }
    deriving (Eq, Show,  Read)


----------------------------------------------------------------------------
-- The statistcs are stored in a text file and read from and updated with 
-- each game. There are differet statisitics for Player vs. Player and
-- Player vs. Computer
----------------------------------------------------------------------------
readStatistics :: IO Statistics
readStatistics = do
    stats <- readFile "statistics.txt"
    let test = (read stats) :: Statistics
    return test

writeStatistics :: Statistics -> IO ()
writeStatistics stats = do
     writeFile "statistics.txt" (show stats)

--Bool is current mode
printStatistics :: Statistics -> Bool -> IO()
printStatistics stats mode = do
    --Player vs Player
    if(mode) then do
        let totalGames = (player1pvpWins stats) + (player2pvpWins stats)
        let p1perc = ((player1pvpWins stats) * 100) `div` totalGames
        let p2perc = ((player2pvpWins stats) * 100) `div` totalGames
        putStrLn("Total Games: " ++ (show totalGames))
        putStrLn("Player 1 has won " ++ (show (player1pvpWins stats)) ++ " games (" ++ (show p1perc) ++ "% win rate)")
        putStrLn("Player 2 has won " ++ (show (player2pvpWins stats)) ++ " games (" ++ (show p2perc) ++ "% win rate)")
    --Player vs Computer
        else do
        let totalGames = (player1pvcWins stats) + (comppvcWins stats)
        let p1perc = ((player1pvcWins stats)* 100)`div`totalGames
        let compPerc = ((comppvcWins stats) * 100)`div`totalGames
        putStrLn("Total Games: " ++ (show totalGames))
        putStrLn("Player 1 has won " ++ (show (player1pvcWins stats)) ++ " games (" ++ (show p1perc) ++ "% win rate)")
        putStrLn("Computer has won " ++ (show (comppvcWins stats)) ++ " games (" ++ (show compPerc) ++ "% win rate)")
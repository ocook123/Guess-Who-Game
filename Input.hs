module Input where

import System.Directory

findGender :: String -> IO String
findGender prompt = do 
  putStrLn prompt
  gender <- getLine
  if (gender == "Male" || gender == "Female") then
    return gender
  else 
    do 
      putStrLn "Error: Invalid gender"
      findGender prompt

findDept :: String -> IO String
findDept prompt = do 
  putStrLn prompt
  dept <- getLine
  if (dept == "CS" || dept == "ECE") then
    return dept
  else 
    do 
      putStrLn "Error: Invalid Department"
      findDept prompt

findPhone :: String -> IO String
findPhone prompt = do 
  putStrLn prompt
  phone <- getLine
  if (phone == "Android" || phone == "iPhone") then
    return phone
  else 
    do 
      putStrLn "Error: Invalid Phone"
      findPhone prompt

findGlasses :: String -> IO String
findGlasses prompt = do 
  putStrLn prompt
  glass <- getLine
  if (glass == "Y" || glass == "N") then
    return glass
  else 
    do 
      putStrLn "Error: Invalid Choice"
      findGlasses prompt

findCrypto :: String -> IO String
findCrypto prompt = do 
  putStrLn prompt
  cry <- getLine
  if (cry == "Y" || cry == "N") then
    return cry
  else 
    do 
      putStrLn "Error: Invalid Choice"
      findCrypto prompt

checkName :: String -> IO String
checkName prompt = do
  putStrLn prompt
  nm <- getLine
  let testName = nm ++ ".txt"
  boolCheck <- doesFileExist testName
  if (boolCheck) then do
    putStrLn "That name already exists. Try again"
    putStrLn (show boolCheck)
    checkName prompt
  else 
    return nm
  
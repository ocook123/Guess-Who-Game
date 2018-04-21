module AddCharacter where

import Character

main :: IO ()
main = do
  -- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
  putStrLn "\nCreating Character"
  putStrLn "Please enter the Character's name"
  nameChar <- getLine
  gen <- findGender "Please enter the Character's gender (Male or Female)"
  dep <- findDept "Please enter the Character's departement (CS or ECE)"
  pho <- findPhone "Please enter the Character's phone preference (Android or iPhone)"
  putStrLn "Please enter the Character's country of origin"
  cont <- getLine
  glas <- findGlasses "Please enter if the Character wears glasses (Y or N)"
  cc <- findCrypto "Please enter if the Character owns cryptocurrency (Y or N)"
  let charname = nameChar
  let chargen = if(gen == "Male") then True else False 
  let chardep = if(dep == "CS") then True else False
  let charpho = if(pho == "Android") then True else False
  let charcout = cont
  let charglas = if(glas == "Y") then True else False
  let charcc = if(cc == "Y") then True else False
  let inputChar = (Character{ name = charname, gender = chargen, dept = chardep, phone = charpho, country = charcout, glasses = charglas, cryptocurr = charcc}) :: Character
  putStrLn (printChar inputChar)
  let fileName = (name inputChar)
  let filePath = fileName ++ ".txt"
  writeFile filePath (show inputChar)
  let filePathwithEnd = filePath ++ "\n"
  appendFile "listOfFiles.txt" filePathwithEnd


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
      putStrLn "Error: Invalid Department"
      findPhone prompt

findGlasses :: String -> IO String
findGlasses prompt = do 
  putStrLn prompt
  glass <- getLine
  if (glass == "Y" || glass == "N") then
    return glass
  else 
    do 
      putStrLn "Error: Invalid Department"
      findGlasses prompt

findCrypto :: String -> IO String
findCrypto prompt = do 
  putStrLn prompt
  cry <- getLine
  if (cry == "Y" || cry == "N") then
    return cry
  else 
    do 
      putStrLn "Error: Invalid Department"
      findCrypto prompt
module AddCharacter where

import Character
import Input
import System.Directory

main :: IO ()
main = do
  -- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
  putStrLn "\nCreating Character"
  nameChar <- checkName "Please enter the Character's name"
  gen <- findGender "Please enter the Character's gender (Male or Female)"
  dep <- findDept "Please enter the Character's departement (CS or ECE)"
  pho <- findPhone "Please enter the Character's phone preference (Android or iPhone)"
  putStrLn "Please enter the Character's country of origin"
  cont <- getLine
  glas <- findGlasses "Please enter if the Character wears glasses (Y or N)"
  cc <- findCrypto "Please enter if the Character owns cryptocurrency (Y or N)"
  let charname = nameChar
  let chargen = gen == "Male"
  let chardep = dep == "CS"
  let charpho = pho == "Android"
  let charcout = cont
  let charglas = glas == "Y"
  let charcc = cc == "Y"
  let inputChar = (Character{ name = charname, gender = chargen, dept = chardep, phone = charpho, country = charcout, glasses = charglas, cryptocurr = charcc}) :: Character
  putStrLn (printChar inputChar)
  let fileName = (name inputChar)
  let filePath = fileName ++ ".txt"
  writeFile filePath (show inputChar)
  let filePathwithEnd = filePath ++ "\n"
  appendFile "listOfFiles.txt" filePathwithEnd

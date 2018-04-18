import System.Environment
import Control.Monad.State.Lazy
import Character
import Data.Maybe (listToMaybe)




main :: IO ()
main = do
  l <- readFile "/Users/oliviacook/MyPLCRepo/ocook/projectTest/test.txt"
  putStr l
  -- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
  let testing = "Character: Professor Stump \n Gender: M \n Department: CS \n Phone: Android \n Contry: USA \n Wears Glasses: T \n Holds Crypto: T \n"
  --appendFile "/Users/oliviacook/MyPLCRepo/ocook/projectTest/return.txt" testing
  {-let testChar = Character{ name = "Char1", gender = True, dept = True, phone = True, country = "test", glasses = True, cryptocurr = True}
  let testChar2 = Character{ name = "Char2", gender = False, dept = False, phone = False, country = "test2", glasses = False, cryptocurr = False}
  let z = [testChar, testChar2]
  putStrLn (show z)
  let xs = removingPhone z False
  putStrLn (show xs)-}
  putStrLn "Creating Character"
  putStrLn "Please enter the Character's name"
  name <- getLine
  putStrLn "Please enter the Character's gender (Male or Female)"
  gen <- getLine
  putStrLn "Please enter the Character's departement (CS or ECE)"
  dep <- getLine
  putStrLn "Please enter the Character's phone preference (Android or iPhone)"
  pho <- getLine
  putStrLn "Please enter the Character's country of origin"
  cont <- getLine
  putStrLn "Please enter if the Character wears glasses (Y or N)"
  glas <- getLine
  putStrLn "Please enter if the Character owns cryptocurrency (Y or N)"
  cc <- getLine
  let allProp = True
  let charname = name
  let chargen = if(gen == "Male") 
                      then True
                      else False 
  let chardep = if(dep == "CS") 
                  then True 
                  else False
  let charpho = if(pho == "Android") 
                  then True 
                  else False
  let charcout = cont
  let charglas = if(glas == "Y") 
                  then True 
                  else False
  let charcc = if(cc == "Y") 
                  then True 
                  else False
  let inputChar = Character{ name = charname, gender = chargen, dept = chardep, phone = charpho, country = charcout, glasses = charglas, cryptocurr = charcc}
  appendFile "/Users/oliviacook/MyPLCRepo/ocook/projectTest/return.txt" (show inputChar)
  putStrLn (show inputChar)
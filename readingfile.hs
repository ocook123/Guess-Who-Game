import System.Environment
import Control.Monad.State.Lazy
import Character



main :: IO ()
main = do
  l <- readFile "/Users/oliviacook/MyPLCRepo/ocook/projectTest/test.txt"
  putStr l
  -- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
  let testing = "Character: Professor Stump \n Gender: M \n Department: CS \n Phone: Android \n Contry: USA \n Wears Glasses: T \n Holds Crypto: T \n"
  appendFile "/Users/oliviacook/MyPLCRepo/ocook/projectTest/return.txt" testing
  let testChar = Character{gender = True, dept = True, phone = True, country = "test", glasses = True, cryptocurr = True}
  let testChar2 = Character{gender = False, dept = False, phone = False, country = "test2", glasses = False, cryptocurr = False}
  let z = [testChar, testChar2]
  putStrLn (show z)
  let xs = removingGender z True
  putStrLn (show xs)
  
  
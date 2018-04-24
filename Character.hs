{-# LANGUAGE ScopedTypeVariables #-}
module Character where

import Input

-- Structure - Gender, Dept, Phone Pref, Country, Wears Glasses, Holds Crypto
-- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
data Character = Character { name :: String , gender :: Bool , dept :: Bool , phone :: Bool , country :: String , glasses :: Bool , cryptocurr :: Bool }
    deriving (Eq, Show,  Read)

-- printChar & toString basically do the same thing
printChar :: Character -> String
printChar (Character a b c d e f g) = a ++ ": " ++ (if b == True then "Male" else "Female") 
            ++ ", " ++ (if c == True then "CS, " else "ECE, ") ++ (if d == True then "Android, " else "iPhone, ") ++ 
            e ++ ", " ++ (if f == True then "Wears Glasses, " else "Doesn't Wear Glasses, ") ++
            (if g == True then "Owns Crypto$" else "Doesn't Own Crypto$") ++ "\n"

toString :: Character -> String
toString (Character name gender dept phone country glasses crypto) = "Name: " ++ name ++ "\tGender: " ++ genderString gender ++ "\tDept: " ++
            deptString dept ++ "\tCountry: " ++ country ++ "\nPhone: " ++ phoneString phone ++ "\t\tGlasses: " ++ yesnoString glasses ++ "\t\tCryptocurrency: " ++ 
            yesnoString crypto 
    
genderString :: Bool -> String
genderString True = "Male"
genderString False = "Female"

deptString :: Bool -> String
deptString True = "CS"
deptString False = "ECE"

phoneString :: Bool -> String 
phoneString True = "Android"
phoneString False = "iPhone"

yesnoString :: Bool -> String
yesnoString True = "Yes"
yesnoString False = "No"


removeAllNamesBut :: [Character] -> String -> [Character]
removeAllNamesBut [] _ = []
removeAllNamesBut (x : y) n = if (name x) == n then
                (x : []) else removeAllNamesBut y n

keepAllNamesBut :: [Character] -> String -> [Character]
keepAllNamesBut [] _ = []
keepAllNamesBut (x : y) n = if (name x) == n then
            keepAllNamesBut y n else x : keepAllNamesBut y n

removeGender :: [Character] -> Bool -> [Character]
removeGender [] _ = []
removeGender (x : y) b = if (gender x) == b then 
    removeGender y b else
    x : removeGender y b

removeDept :: [Character] -> Bool -> [Character]
removeDept [] _ = []
removeDept (x : y) b = if (dept x) == b then 
    (removeDept y b) else
    x : removeDept y b

removePhone :: [Character] -> Bool -> [Character]
removePhone [] _ = []
removePhone (x : y) b = if (phone x)== b then 
    (removePhone y b) else
    x : removePhone y b

removeCountry :: [Character] -> String -> [Character]
removeCountry [] _ = []
removeCountry (x : y) b = if (country x) == b then 
    removeCountry y b else
    x : removeCountry y b

keepCountry :: [Character] -> String -> [Character]
keepCountry [] _ = []
keepCountry (x : y) b = if (country x) == b then 
    x : keepCountry y b else
    keepCountry y b

removeGlasses :: [Character] -> Bool -> [Character]
removeGlasses [] _ = []
removeGlasses (x : y) b = if (glasses x) == b then 
    (removeGlasses y b) else
    x : removeGlasses y b

removeCrypto :: [Character] -> Bool -> [Character]
removeCrypto [] _ = []
removeCrypto (x : y) b = if (cryptocurr x) == b then 
    (removeCrypto y b) else
    x : removeCrypto y b

{-instance Read Character where
    readsPrec _ value =  (Character a b c d e f g) = "(" ++ a ++ "," ++ read b ++ "," ++ read c ++ "," ++ read d ++ "," ++ e ++ "," ++ read f ++ "," ++ read g ++ ")"-}


    
-- Int: question number entered 
-- Character: The character this player is trying to guess
-- [Character]: Remaining characters
-- Return: [Character] with eliminations made
handleQuestion :: Int -> Character -> [Character] -> IO [Character]
handleQuestion x solution c = case x of
    0 -> do -- If the guess is correct, every wrong character is removed from the list
        putStrLn "Guess the name:"
        n <- getLine 
        if (name solution) == n then do
            putStrLn ("The character is " ++ n)
            let newCharacters = removeAllNamesBut c n
            return newCharacters
        else do
            putStrLn ("The character is not " ++ n)
            let newCharacters = keepAllNamesBut c n
            return newCharacters
    1 -> do
        g <- findGender "Enter a gender:" 
        let b = (gender solution)
        let output = if b then "Male" else "Female"
        putStrLn ("The character is a " ++ output)
        let newCharacters = removeGender c (not b)
        return newCharacters
    2 -> do
        d <- findDept "Enter a department:" 
        let b = (dept solution)
        let output = if b then "CS" else "ECE"
        putStrLn ("The character is part of the " ++ output ++ " department")
        let newCharacters = removeDept c (not b)
        return newCharacters
    3 -> do
        putStrLn "Enter a country name:"
        co <- getLine 
        if (country solution) == co then do
            putStrLn ("The character is from " ++ co)
            let newCharacters = keepCountry c co
            return newCharacters
        else do
            putStrLn ("The character is not from " ++ co)
            let newCharacters = removeCountry c co
            return newCharacters
    4 -> do
        p <- findPhone "Enter a phone type:" 
        let b = (phone solution)
        let output = if b then "Android" else "iPhone"
        putStrLn ("The character's phone type is " ++ output)
        let newCharacters = removePhone c (not b)
        return newCharacters
    5 -> do
        g <- findGlasses "Enter 'Y' if you think the character has glasses. Enter 'N' otherwise:" 
        let b = (glasses solution)
        let output = if b then "does" else "does not"
        putStrLn ("\nThe character " ++ output ++ " wear glasses\n")
        let newCharacters = removeGlasses c (not b)
        return newCharacters
    6 -> do
        cr <- findCrypto "Enter 'Y' if you think the character owns cryptocurrency. Enter 'N' otherwise:" 
        let b = (cryptocurr solution)
        let output = if b then "does not" else "does"
        putStrLn ("\nThe character " ++ output ++ " own cryptocurrency\n")
        let newCharacters = removeCrypto c (not b)
        return newCharacters
    _ -> do 
        putStrLn "Invalid question selected"
        return c
{-# LANGUAGE ScopedTypeVariables #-}
module Character where

    -- Structure - Gender, Dept, Phone Pref, Country, Wears Glasses, Holds Crypto
    -- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
    data Character = Character { name :: String , gender :: Bool , dept :: Bool , phone :: Bool , country :: String , glasses :: Bool , cryptocurr :: Bool }
        --deriving Show

    removingGender :: [Character] -> Bool -> [Character]
    removingGender [] _ = []
    removingGender (x : y) b = if (gender x) == b then 
        removingGender y b else
        x : removingGender y b

    removingDept :: [Character] -> Bool -> [Character]
    removingDept [] _ = []
    removingDept (x : y) b = if (dept x) == b then 
        (removingDept y b) else
        x : removingDept y b

    removingPhone :: [Character] -> Bool -> [Character]
    removingPhone [] _ = []
    removingPhone (x : y) b = if (phone x)== b then 
        (removingPhone y b) else
        x : removingPhone y b

    removingCountry :: [Character] -> String -> [Character]
    removingCountry [] _ = []
    removingCountry (x : y) b = if (country x) == b then 
        (removingCountry y b) else
        x : removingCountry y b

    removingGlasses :: [Character] -> Bool -> [Character]
    removingGlasses [] _ = []
    removingGlasses (x : y) b = if (glasses x) == b then 
        (removingGlasses y b) else
        x : removingGlasses y b

    removingCrypto :: [Character] -> Bool -> [Character]
    removingCrypto [] _ = []
    removingCrypto (x : y) b = if (cryptocurr x) == b then 
        (removingCrypto y b) else
        x : removingCrypto y b

     
    instance Show Character where 
        show (Character a b c d e f g) = a ++ ": " ++ (if b == True then "Male, " else "Female, ") 
            ++ (if c == True then "CS, " else "ECE, ") ++ (if d == True then "Android, " else "iPhone, ") ++
            "From " ++ e ++ (if f == True then ", Wears Glasses, " else ", Doesn't Wear Glasses, ") ++ 
            (if g == True then "Owns Crypto$" else "Doesn't Own Crypto$") ++ "\n"
    
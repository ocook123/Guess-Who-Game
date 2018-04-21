{-# LANGUAGE ScopedTypeVariables #-}
module Character where

<<<<<<< HEAD
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
            deptString dept ++ "\tCountry: " ++ country ++ "\nPhone: " ++ phoneString phone ++ "\tGlasses: " ++ yesnoString glasses ++ "\tCryptocurrency: " ++ 
            yesnoString crypto 
    
genderString :: Bool -> String
genderString True = "Male"
genderString False = "False"

deptString :: Bool -> String
deptString True = "CS"
deptString False = "ECE"

phoneString :: Bool -> String 
phoneString True = "Android"
phoneString False = "iPhone"

yesnoString :: Bool -> String
yesnoString True = "Yes"
yesnoString False = "No"


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
    (removeCountry y b) else
    x : removeCountry y b

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
=======
    -- Structure - Gender, Dept, Phone Pref, Country, Wears Glasses, Holds Crypto
    -- Gender (M = T, F = F), Dept (CS = T, ECE = F), Phone (Android = T, iPhone = F)
    data Character = Character { name :: String , gender :: Bool , dept :: Bool , phone :: Bool , country :: String , glasses :: Bool , cryptocurr :: Bool }
        deriving (Eq, Show, Read)

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

    printChar :: Character -> String
    printChar (Character a b c d e f g) = a ++ ": " ++ (if b == True then "Male" else "Female") 
                ++ ", " ++ (if c == True then "CS, " else "ECE, ") ++ (if d == True then "Android, " else "iPhone, ") ++ 
                e ++ ", " ++ (if f == True then "Wears Glasses, " else "Doesn't Wear Glasses, ") ++
                (if g == True then "Owns Crypto$" else "Doesn't Own Crypto$") ++ "\n"

    printCharList :: [Character] -> String
    printCharList [] = []
    printCharList (char : rest) = printChar char ++ printCharList rest
>>>>>>> origin/master

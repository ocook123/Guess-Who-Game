module OptimalPick where

import Character

--takes in the list of characters, finds the optimal pick, removes, returns updated list
--optimal pick - the property that is reflected most evenly in the list
pickProperty :: [Character] -> Character -> [Character]
pickProperty chars  findChar = 
    let genderList = makeGenderList chars in
    let deptList = makeDeptList chars in
    let phoneList = makePhoneList chars in
    let glassesList = makeGlassesList chars in
    let ccList = makeCCList chars in
    let gendTrue = abs (((length chars) `div` 2) - findTotalTrues genderList) in
    let depTrue = abs (((length chars) `div` 2) - findTotalTrues deptList) in
    let phoTrue = abs (((length chars) `div` 2) - findTotalTrues phoneList) in
    let glasTrue = abs (((length chars) `div` 2) - findTotalTrues glassesList) in
    let ccTrue = abs (((length chars) `div` 2) - findTotalTrues ccList) in
    let trueList = minimum [gendTrue, depTrue, phoTrue, glasTrue, ccTrue] in
    if(gendTrue == trueList) then
        if (gender findChar) then removeGender chars False else removeGender chars True
    else
        if(depTrue == trueList) then
            if (dept findChar) then removeDept chars False else removeDept chars True
        else
            if(phoTrue == trueList) then
                if (phone findChar) then removePhone chars False else removePhone chars True
            else
                if(glasTrue == trueList) then
                    if (glasses findChar) then removeGlasses chars False else removeGlasses chars True
                else 
                    if (cryptocurr findChar) then removeCrypto chars False else removeCrypto chars True
                                    

makeGenderList :: [Character] -> [Bool]
makeGenderList [] = []
makeGenderList (char : nextChars) = (gender char) : (makeGenderList nextChars)

makeDeptList :: [Character] -> [Bool]
makeDeptList [] = []
makeDeptList (char : nextChars) = (dept char) : (makeDeptList nextChars)

makePhoneList :: [Character] -> [Bool]
makePhoneList [] = []
makePhoneList (char : nextChars) = (phone char) : (makePhoneList nextChars)

makeGlassesList :: [Character] -> [Bool]
makeGlassesList [] = []
makeGlassesList (char : nextChars) = (glasses char) : (makeGlassesList nextChars)

makeCCList :: [Character] -> [Bool]
makeCCList [] = []
makeCCList (char : nextChars) = (cryptocurr char) : (makeCCList nextChars)

findTotalTrues :: [Bool] -> Int
findTotalTrues [] = 0
findTotalTrues (first : rest) = if (first == True) then (1 + findTotalTrues rest) else findTotalTrues rest

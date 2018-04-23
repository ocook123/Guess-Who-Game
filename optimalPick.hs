module OptimalPick where

import Character

--takes in the list of characters, finds the optimal pick, removes, returns updated list
pickProperty :: [Character] -> [Character]
pickProperty chars = 
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
        if (gendTrue > ((length chars) `div` 2)) then removeGender chars True else removeGender chars False
    else
        if(depTrue == trueList) then
            if (depTrue > ((length chars) `div` 2)) then removeDept chars True else removeDept chars False
        else
            if(phoTrue == trueList) then
                if (phoTrue > ((length chars) `div` 2)) then removePhone chars True else removePhone chars False
            else
                if(glasTrue == trueList) then
                    if (glasTrue > ((length chars) `div` 2)) then removeGlasses chars True else removeGlasses chars False
                else 
                    if (ccTrue > ((length chars) `div` 2)) then removeCrypto chars True else removeCrypto chars False
                                    

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

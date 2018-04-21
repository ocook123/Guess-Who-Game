The Plan:

Each character will be stored in their own file
Another file will contain a list of the paths to each character file
Haskell has a method to create a file
When adding a character, information is taken in from command line and it’s own file is created and that path is appended to the file with the paths
When reading in the characters, the file with the paths is read in and for each line, we read the character and append it to a list of characters which is what is created

I still have to add the check if there is already a character with the name that is entered already exists

Files:
readingCharacter.hs - reads in characters
addingCharacter.hs - adds character
listOfFiles.txt - list of paths to character Files
Kenneth.txt - character file
Olivia.txt - character file
Character.hs - character data type
CharacterInput.txt - some professor's info compiled so far but does not interact with the program
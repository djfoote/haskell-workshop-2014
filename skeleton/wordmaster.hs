import System.IO
import System.Random
import Data.List(nub)








































turnPrompt = "Enter a word to get its score or q to quit."

--                   ======================================                   --
--                   == THIS IS YOUR ABSTRACTION BARRIER ==                   --
--                   ======================================                   --

-- I may or may not get around to writing explanations for what's going on here.
-- If you read the chapter on IO in LYAH, this should all make sense.

startPrompt word = "Playing with a " ++ (show (length word)) ++ " letter word."


startGame :: [Word] -> Word -> IO ()
startGame validGuesses word = do
    putStrLn (startPrompt word)
    takeTurn (Game validGuesses word)

getValidGuesses :: IO [Word]
getValidGuesses = do 
    contents <- readFile "validguesses.txt"
    return (lines contents)

chooseRandomWord :: [Word] -> StdGen -> Word
chooseRandomWord validWords gen = 
    let (index, _) = randomR (0, length validWords) gen
    in  validWords !! index

-- A valid master word has no duplicate letters.
filterValidMasterWords :: [Word] -> [Word]
filterValidMasterWords guesses =
    let uniqueChars = length . nub
        noDuplicates guess = uniqueChars guess == length guess
    in  filter noDuplicates guesses

main = do
    gen <- getStdGen
    validGuesses <- getValidGuesses
    let validMasterWords = filterValidMasterWords validGuesses
        word = chooseRandomWord validMasterWords gen
    startGame validGuesses word

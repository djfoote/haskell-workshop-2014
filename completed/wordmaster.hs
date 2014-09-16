import System.IO
import System.Exit(exitSuccess)
import System.Random
import Control.Monad(when)
import Data.Set(size, fromList)

letters = ['a'..'z']

data GameMessage = BadNumLetters
                 | CorrectWord
                 | NotAWord Word 
                 deriving (Eq)

display :: GameMessage -> String
display BadNumLetters = "Wrong number of letters"
display (NotAWord w)  = w ++ " is not a valid word."
display CorrectWord   = "Congratulations! You win!"

prompt = "Enter a word to get its score or q to quit."

type Word = String
type WordMaster = (Word -> Either GameMessage Int)

numCommonLetters :: Word -> Word -> Int
numCommonLetters goal guess = length (filter (`elem` guess) goal)

makeWordMaster :: [Word] -> Word -> WordMaster
makeWordMaster validGuesses goal guess
    | (length guess) /= (length goal) = Left BadNumLetters
    | guess == goal                   = Left CorrectWord
    | not (guess `elem` validGuesses) = Left (NotAWord guess)
    | otherwise                       = Right (numCommonLetters goal guess)

startGame :: [Word] -> Word -> IO ()
startGame validGuesses word = do
    putStrLn ("Playing with a " ++ (show (length word)) ++ " letter word.")
    let wordMaster = makeWordMaster validGuesses word
    takeTurn wordMaster word

takeTurn :: WordMaster -> Word -> IO ()
takeTurn master word = do
    putStrLn prompt
    guess <- getLine
    when (guess == "q") (do
        putStrLn ("The word was " ++ word)
        exitSuccess)
    evaluateGuess guess (master guess)
    takeTurn master word 

evaluateGuess :: Word -> (Either GameMessage Int) -> IO ()
evaluateGuess _ (Left message) = do
    putStrLn (display message)
    when (message == CorrectWord) 
        exitSuccess
evaluateGuess guess (Right common) = do
    putStrLn ("The word " ++ guess ++ " has " ++ (show common) ++ 
        " letters in common")

--                   ======================================                   --
--                   == THIS IS YOUR ABSTRACTION BARRIER ==                   --
--                   ======================================                   --

-- I may or may not get around to writing explanations for what's going on here.
-- If you read the chapter on IO in LYAH, this should all make sense.

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
    let uniqueChars = size . fromList
        noDuplicates guess = uniqueChars guess == length guess
    in  filter noDuplicates guesses

main = do
    gen <- getStdGen
    validGuesses <- getValidGuesses
    let validMasterWords = filterValidMasterWords validGuesses
        word = chooseRandomWord validMasterWords gen
    startGame validGuesses word

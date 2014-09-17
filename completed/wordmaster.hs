import System.IO
import System.Exit(exitSuccess)
import System.Random
import Control.Monad(when)
import Data.List(nub)

type Word = String

data GameState   = Game [Word] Word
data Result      = Failure Error
                 | Victory
                 | Score Word Int

data Error = BadNumLetters
           | NotAWord Word 

commonLetters :: Word -> Word -> Int
commonLetters goal guess = length (filter (`elem` guess) goal)

checkWord :: GameState -> Word -> Result
checkWord (Game valid goal) guess
    | (length guess) /= (length goal) = Failure BadNumLetters
    | not (guess `elem` valid)        = Failure (NotAWord guess)
    | guess == goal                   = Victory
    | otherwise                       = Score guess (commonLetters goal guess)

feedback :: Result -> String
feedback Victory                   = "Congratulations! You win!"
feedback (Failure (BadNumLetters)) = "Wrong number of letters."
feedback (Failure (NotAWord w))    = w ++ " is not a valid word."
feedback (Score guess common)      = 
    guess ++ " has " ++ (show common) ++ " letters in common."

takeTurn :: GameState -> IO ()
takeTurn state@(Game _ goal) = do
    putStrLn turnPrompt
    guess <- getLine
    when (guess == "q") (do
        putStrLn ("The word was " ++ goal)
        exitSuccess)
    let result = checkWord state guess
    putStrLn (feedback result)
    case result of Victory -> exitSuccess
                   _       -> takeTurn state

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

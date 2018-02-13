module Main where

import Control.Monad (forever)
import Data.Char (isLower, isAlpha, isAscii)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return $ filter (\w -> gameLength w && gameChars w) aw
    where gameLength w = 
            let l = length (w :: String)
            in     l >= minWordLength
                && l <= maxWordLength
          gameChars w =
            all (\c -> isAlpha c && isAscii c && isLower c) w

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

gameWord :: IO String
gameWord = gameWords >>= randomWord

data Puzzle = 
    Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed wrong) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed
        ++ " Wrong guesses: " ++ show wrong

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (nts w) [] 0
    where nts w = fmap (\_ -> Nothing) w

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word
                   filledInSoFar s w) c =
    Puzzle word newFilledInSoFar (c : s) newWrong
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar =
            zipWith (zipper c)
                word filledInSoFar
          newWrong = if charInWord p c
                     then w
                     else w + 1

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess,
          alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word!"
            return $ fillInCharacter puzzle guess
        (False, _) -> do
            putStrLn "Nope, not in the word!"
            return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ wrong) = 
    if wrong > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then do
        putStrLn "You win!"
        exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $
        "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single letter!"

main :: IO ()
main = do
    word <- gameWord
    let puzzle = freshPuzzle word
    runGame puzzle

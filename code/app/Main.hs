module Main where

import Sha256 (sha256)
import Control.Monad (when)
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering))
import GHC.IO.Handle.FD ( stdout )


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn "SHA-256"
    putStrLn ""
    putStrLn ""
    putStrLn "This program hashes any string or file using SHA-256."
    putStrLn ""
    prompt

prompt :: IO ()
prompt = do
    putStrLn "Select an option to hash:"
    putStrLn "1) string"
    putStrLn "2) file"
    putStrLn "3) quit"
    ans <- getLine
    when (ans == "1") $ do
        putStrLn ""
        putStr "Enter a string that you want to hash:"
        putStrLn ""
        string <- getLine
        putStrLn ""
        putStrLn $ "Hashing: " ++ string
        putStrLn "--"
        putStr (sha256 string)
        putStrLn ""
        putStrLn ""
        putStrLn ""
        promptAgain
    when (ans == "2") $ do
        putStrLn ""
        putStrLn "Please enter the file that you want to hash: "
        file <- getLine
        contents <- readFile file
        putStrLn ""
        putStrLn $ "Hashing: " ++ file 
        putStrLn "--"
        putStrLn (sha256 contents)
        putStrLn ""
        putStrLn ""
        putStrLn ""
        promptAgain
    when (ans == "3") $ return ()
    when (ans `notElem` ["1","2","3"]) $ do
        putStrLn ""
        putStrLn "Incorrect input.  Try again"
        putStrLn ""
        putStrLn ""
        prompt

promptAgain :: IO ()
promptAgain = do 
    putStrLn "Do you want to hash something else? (y/n)"
    yn <- getLine
    putStrLn ""
    when (yn `elem` ["Y","y"]) prompt



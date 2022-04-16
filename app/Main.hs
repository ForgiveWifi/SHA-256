module Main where

import SHA256 (sha256)
import Control.Monad (when)
import qualified Data.Text.IO as IO (readFile)


main :: IO ()
main = do
    putStrLn "SHA-256 or Secure Hash Algorithm"
    putStrLn "This is program hashes any string or file"
    putStrLn "Select an option.  Would you like to hash a :"
    putStrLn ""
    putStrLn "1) string"
    putStrLn "2) file"
    putStrLn "Enter q to quit"
    ans <- getLine 
    when (ans == "1") $ putStr (sha256 ans) 
    when (ans == "2") $ do 
        putStrLn "Please enter the file that you want to hash:" 
        file <- getLine 
        contents <- IO.readFile file 
        putStrLn (sha256 contents)
    when (ans == "q") $ return ()
    main

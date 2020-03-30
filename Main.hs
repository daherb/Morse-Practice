module Main where

import System.Environment

import Text
import Sound

-- All supported letters
letters = "abcdefghijklmnopqrstuvxyzåäößü1234567890?!,.-()@/%\"';:=+√ "
-- letters = "adehilortvxzö "

usage :: IO ()
usage =
  do
    prg <- getProgName
    putStrLn $ unwords [prg++":","lang","no.words","wav-file","(letters)","(txt-file)"]
    putStrLn "\nwhere"
    putStrLn "\tlang: language of the wordlist, i.e. en, de or sv"
    putStrLn "\tno.words: number of words in the text to be created"
    putStrLn "\twav-file: the output file to be written"
    putStrLn "\tletters: optional list of letters that should be practiced. If this list contains punctuation or digits, they will be randomly generated between words"
    putStrLn "\ttxt-file: optional output file for the text"

main :: IO ()
main =
  do
    args <- getArgs
    if length args < 3 then usage
      else
      do
        let ct = read (args !! 1)
        let ltrs = if length args >= 4 then (args !! 3) else letters
        dict <- loadDict (args !! 0 ++ ".txt") ltrs
        let wav = args !! 2
        text <- sampleText dict ltrs ct
        textToMorse text wav
        if length args == 5 then writeFile (args !! 4) text else return ()

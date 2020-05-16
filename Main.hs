module Main where

import System.Environment

import Text
import Sound

-- All supported letters
letters = "abcdefghijklmnopqrstuvxyzåäößü1234567890?!,.-()@/%\"';:=+√ "
-- letters = "adehilortvxzö "

rate = 40

usage :: IO ()
usage =
  do
    prg <- getProgName
    putStrLn $ unwords [prg++":","lang","no.words","wav-file","(letter rate)","(break-rate)","(txt-file)","(letters)"]
    putStrLn "\nwhere"
    putStrLn "\tlang: language of the wordlist, i.e. en, de or sv"
    putStrLn "\tno.words: number of words in the text to be created"
    putStrLn "\twav-file: the output file to be written"
    putStrLn "\tletter rate: optional speed of letters in characters per minute"
    putStrLn "\tbreak rate: optional speed of breaks in characters per minute"
    putStrLn "\ttxt-file: optional output file for the text"
    putStrLn "\tletters: optional list of letters that should be practiced."
    putStrLn "\t\tIf this list contains punctuation or digits,"
    putStrLn "\t\tthey will be randomly generated between words"

main :: IO ()
main =
  do
    args <- getArgs
    if length args < 3 then usage
      else
      do
        let ct = read (args !! 1)
        let letter_rate = if length args >= 4 then (read $ args !! 3) else rate
        let break_rate = if length args >= 5 then (read $ args !! 4) else rate
        let ltrs = if length args == 7 then (args !! 6) else letters
        dict <- loadDict (args !! 0 ++ ".txt") ltrs
        let wav = args !! 2
        text <- sampleText dict ltrs ct
        textToMorse letter_rate break_rate text wav
        if length args >= 6 then writeFile (args !! 5) text else return ()

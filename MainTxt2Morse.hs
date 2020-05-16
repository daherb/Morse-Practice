module Main where

import System.Environment

import Text
import Sound

rate = 40

usage :: IO ()
usage =
  do
    prg <- getProgName
    putStrLn $ unwords [prg++":","txt-file","wav-file","(letter rate)","(break-rate)"]
    putStrLn "\nwhere"
    putStrLn "\ttxt-file: the input file for the text"
    putStrLn "\twav-file: the output file to be written"
    putStrLn "\tletter rate: optional speed of letters in characters per minute"
    putStrLn "\tbreak rate: optional speed of breaks in characters per minute"

main :: IO ()
main =
  do
    args <- getArgs
    if length args < 2 then usage
      else
      do
        let letter_rate = if length args >= 3 then (read $ args !! 2) else rate
        let break_rate = if length args >= 4 then (read $ args !! 3) else rate
        let wav = args !! 1
        text <- readFile (args !! 0)
        textToMorse letter_rate break_rate text wav


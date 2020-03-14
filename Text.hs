module Morse where

import Test.QuickCheck
import Data.Char

letters = "adehilortvxzö "

loadFile :: FilePath -> IO [String]
loadFile infile =
  do
  content <- lines <$> readFile infile
  return $ filter (all $ flip (elem . toLower) letters) content

sampleText :: [String] -> Int -> IO String
sampleText words count =
  unwords <$> generate (vectorOf count (elements words))
  



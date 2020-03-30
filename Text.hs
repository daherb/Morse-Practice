module Text where

import Test.QuickCheck
import Data.Char
import Data.List

digits = "0123456789"
punc = "?!,.-()@/%\"';:=+√"
special = digits ++ punc

loadDict :: FilePath -> String -> IO [String]
loadDict infile ltrs =
  do
  content <- lines <$> readFile infile
  return $ filter (all $ flip (elem . toLower) ltrs) content

genWord :: [String] -> Gen String
genWord dict = elements dict

genSpecial :: String -> Gen String
genSpecial chars =
  do
    let i = intersect chars special
    c <- elements i
    return $ if null i then [] else [c]

-- Defines what a word is
isWord :: String -> Bool
isWord w = length w > 1 || all isAlpha w

-- Only count words when taking elements from the list
takeNWords :: Int -> [String] -> [String]
takeNWords 0 _ = []
takeNWords c [] = []
takeNWords c (w:ws)
  | isWord w = w:(takeNWords (c-1) ws)
  | otherwise = w:(takeNWords c ws)

-- Skips the first element of a list if it is a special character, i.e. not a word
skipFirstSpecial :: [String] -> [String]
skipFirstSpecial [] = []
skipFirstSpecial (w:ws)
  | isWord w = w:ws
  | otherwise = ws
  
sampleText :: [String] -> String -> Int -> IO String
sampleText dict chars count =
  unwords <$> takeNWords count <$> skipFirstSpecial <$> (generate $ infiniteListOf (frequency [(9,genWord dict),(1,genSpecial chars)]))
  
  



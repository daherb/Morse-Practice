module Text where

import Test.QuickCheck
import Data.Char


loadDict :: FilePath -> String -> IO [String]
loadDict infile ltrs =
  do
  content <- lines <$> readFile infile
  return $ filter (all $ flip (elem . toLower) ltrs) content

sampleText :: [String] -> Int -> IO String
sampleText dict count =
  unwords <$> generate (vectorOf count (elements dict))
  



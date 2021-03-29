-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
-- 
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.
tr :: CharSet -> Maybe CharSet -> String -> String
-- tr substring (Just repSubstring) = map (replaceIfCont substring repSubstring)
tr substring (Just repSubstring) = map (replaceIfCont substring (makeSize repSubstring (length substring)))
tr substring Nothing = filter (`notElem` substring)

makeSize :: String -> Int -> String
makeSize str size
    | length str < size  = str ++ replicate (size - length str) (last str)
    | otherwise  = str

replaceIfCont :: String -> String -> Char -> Char
replaceIfCont substring repSubstring getC   
    | lookup getC (zip substring repSubstring) == Nothing = getC
    | otherwise = let (Just val) = lookup getC (zip substring repSubstring) in val
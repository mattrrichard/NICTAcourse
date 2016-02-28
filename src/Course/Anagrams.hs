{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

anagrams' :: Chars -> List Chars -> List Chars
anagrams' word dictionary =
  intersectBy equalIgnoringCase dictionary (permutations word)


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: Chars -> Filename -> IO (List Chars)
anagrams word dictPath =
  anagrams' word . lines <$> readFile dictPath


-- Compare two strings for equality, ignoring case
equalIgnoringCase :: Chars -> Chars -> Bool
equalIgnoringCase =
  (==) `on` map toLower

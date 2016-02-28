{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`. It
will accept a numeric value as input, representing an amount of money, and
convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and
eleven cents"

Invalid characters should be ignored, meaning that every input string has an
output string. The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output.
There are also functions and data structures that may assist you in deriving the
result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion :: List (Chars -> Chars)
      preillion =
        listh
        [ const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion :: List Chars
      postillion =
        listh
        [ "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh
     [ ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded, Show)

showDigit :: Digit -> Chars
showDigit Zero  = "zero"
showDigit One   = "one"
showDigit Two   = "two"
showDigit Three = "three"
showDigit Four  = "four"
showDigit Five  = "five"
showDigit Six   = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine  = "nine"

showTens :: Digit -> Digit -> Chars
showTens t o =
  case t of
    Zero -> if o /= Zero then showDigit o else ""
    One -> showTeens o
    Two   -> "twenty" ++ tyones
    Three -> "thirty" ++ tyones
    Four  -> "forty" ++ tyones
    Five  -> "fifty" ++ tyones
    Six   -> "sixty" ++ tyones
    Seven -> "seventy" ++ tyones
    Eight -> "eighty" ++ tyones
    Nine  -> "ninety" ++ tyones
  where
    tyones
      | o == Zero = ""
      | otherwise = "-" ++ showDigit o

showTeens :: Digit -> Chars
showTeens Zero  = "ten"
showTeens One   = "eleven"
showTeens Two   = "twelve"
showTeens Three = "thirteen"
showTeens Four  = "fourteen"
showTeens Five  = "fifteen"
showTeens Six   = "sixteen"
showTeens Seven = "seventeen"
showTeens Eight = "eighteen"
showTeens Nine  = "nineteen"

showHundreds :: Digit -> Digit -> Digit -> Chars
showHundreds h t o
  | h == Zero = showTens t o
  | otherwise = showDigit h ++ " hundred" ++
    case showTens t o of
      "" -> ""
      x -> " and " ++ x


showGrouping :: Digit3 -> Chars
showGrouping (D1 o)     = showDigit o
showGrouping (D2 t o)   = showTens t o
showGrouping (D3 h t o) = showHundreds h t o

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

makeGroupings :: List Digit -> List Digit3
makeGroupings digits =
  reverse $ group' (reverse digits)
  where
    group' (o :. t :. h :. ds) = D3 h t o :. group' ds
    group' (o :. t :. Nil) = D2 t o :. Nil
    group' (o :. Nil) = D1 o :. Nil
    group' Nil = Nil

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _ = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"

allZero :: Digit3 -> Bool
allZero (D3 Zero Zero Zero) = True
allZero (D2 Zero Zero) = True
allZero (D1 Zero) = True
allZero _ = False


dollars :: Chars -> Chars
dollars cheque =
  dollarWords ++ showCents centsGroup
  where
    dollarWords =
      case concatSpaces $ map showGroup dillions of
        "" -> "zero dollars"
        "one" -> "one dollar"
        x -> x ++ " dollars"

    concatSpaces (s :. s1 :. ss) = s ++ " " ++ concatSpaces (s1 :. ss)
    concatSpaces (s :. Nil) = s
    concatSpaces Nil = Nil

    showGroup (group, ill)
      | ill == Nil = showGrouping group
      | otherwise = showGrouping group ++ " " ++ ill

    dillions = filter (not . allZero . fst) $ reverse $ zip (reverse dollarGroups) illion
    (ds', cs') = break (== '.') cheque
    dollarGroups = makeGroupings $ listOptional fromChar ds'

    cents = take 2 $ listOptional fromChar cs'
    centsGroup =
      case cents of
        Zero :. o :. Nil -> D1 o
        t :. Nil -> D2 t Zero
        t :. o :. Nil -> D2 t o
        _ -> D1 Zero

showCents :: Digit3 -> Chars
showCents d =
  case showGrouping d of
    "" -> " and zero cents"
    x -> " and " ++ x ++ pluralize " cent" (d :. Nil)

pluralize :: Chars -> List Digit3 -> Chars
pluralize word (g :. Nil) =
  case g of
    D3 Zero Zero One -> word
    D2 Zero One -> word
    D1 One -> word
    _ -> word ++ "s"
pluralize word Nil = word ++ "s"
pluralize word (g :. gs) =
  if allZero g then
    pluralize word gs
  else
    word ++ "s"

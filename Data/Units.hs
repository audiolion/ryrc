-- | This module provides definitions for Measurement System Units and ways to parse
-- text into data so that it can be converted and then convert it back to Strings for
-- output. The Ryrc bot uses this module to do the heavy lifting for unit conversions
module Data.Units where

import Data.List
import Data.Char
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad

type Prefix = String
type Power  = Rational
type Abbr  = String

type PrefixError = String

definitions :: [(Prefix, Power, Abbr)]
definitions =
  [("yocto", 1e-24, "y")
  ,("zepto", 1e-21, "z")
  ,("atto",  1e-18, "a")
  ,("femto", 1e-15, "f")
  ,("pico",  1e-12, "p")
  ,("nano",  1e-9 , "n")
  ,("micro", 1e-6 , "u")
  ,("milli", 1e-3 , "m")
  ,("centi", 1e-2 , "c")
  ,("deci",  1e-1 , "d")
  ,("deca",  1e+1 , "da")
  ,("hecto", 1e2  , "h")
  ,("kilo",  1e3  , "k")
  ,("mega",  1e6  , "M")
  ,("giga",  1e9  , "G")
  ,("tera",  1e12 , "T")
  ,("peta",  1e15 , "P")
  ,("exa",   1e18 , "E")
  ,("zetta", 1e21 , "Z")
  ,("yotta", 1e24 , "Y")

    -- binary_prefixes
  ,("kibi", 2^10, "Ki")
  ,("mebi", 2^20, "Mi")
  ,("gibi", 2^30, "Gi")
  ,("tebi", 2^40, "Ti")
  ,("pebi", 2^50, "Pi")
  ,("exbi", 2^60, "Ei")
  ,("zebi", 2^70, "Zi")
  ,("yobi", 2^80, "Yi")

    -- base_prefixes
  ,("meter",   1e0, "")
  ,("gram",    1e0, "")
  ,("liter",   1e0, "")
  ,("candela", 1e0, "")
  ,("ampere",  1e0, "")
  ,("kelvin",  1e0, "")
  ,("second",  1e0, "")
  ,("mole",    1e0, "")
  ]

-- | 'MetricBaseUnit' provides an enumeration for the base metric unit types
data MetricBaseUnit
  = Meter
  | Gram 
  | Second
  | Kelvin
  | Liter
  | Mole
  | Ampere
  | Candela
  deriving (Show, Eq)

-- | 'Unit' provides a wrapper for base units to include an appropriate prefix and abbreviation
data Unit = M Prefix MetricBaseUnit Abbr
          deriving (Show, Eq)

-- | 'Measurement' Wraps a double value to be associated with a unit and its power
data Measurement  = MetricMeasurement Double Unit Power
                  | Unknown String
                  deriving (Show, Eq)

-- | Gets the 3-ple associated with a given prefix string so long as it is associated with
-- a prefix in the definitions list
findByPrefix :: Prefix -> (Prefix, Power, Abbr)
findByPrefix str = findByPrefix' str definitions
    where 
    findByPrefix' str (x:xs)
        | str == first x  = x
        | otherwise     = findByPrefix' str xs

strToMeasurement :: String -> Measurement
strToMeasurement str = MetricMeasurement (parseDouble str) (M (first (a, b, c)) (parseBase str) (third (a,b,c))) (second (a,b,c))
    where
    (a, b, c) = parseInput str

first :: (a, b, c) -> a
first (a,_,_) = a

second :: (a, b, c) -> b
second (_,b,_) = b

third :: (a, b, c) -> c
third (_,_,c) = c

-- | Used for conversion string input from user intended to be of the form
-- <Double> <currentunit> to <desiredunit>. Function pulls out the <currentunit>
-- and puts it in the Unit data constructor.
parseInput :: String -> (Prefix, Power, Abbr)
parseInput str = (prefix, power, abbr)
    where 
    (prefix, power, abbr) = findByPrefix (takeWhile (not . isSpace) (dropWhile (`notElem` ['a'..'z']) str))

-- | Function looks at an input string and matches a metric base unit to return
parseBase :: String -> MetricBaseUnit
parseBase str
    | "meter" `isInfixOf` str   = Meter
    | "gram" `isInfixOf` str    = Gram
    | "ampere" `isInfixOf` str  = Ampere
    | "candela" `isInfixOf` str = Candela
    | "liter" `isInfixOf` str   = Liter
    | "mole" `isInfixOf` str    = Mole
    | "kelvin" `isInfixOf` str  = Kelvin
    | "second" `isInfixOf` str  = Second

getBaseAbbr :: MetricBaseUnit -> String
getBaseAbbr base
    | base == Meter   = "m"
    | base == Gram    = "g"
    | base == Liter   = "l"
    | base == Second  = "s"
    | base == Mole    = "mol"
    | base == Candela = "cd"
    | base == Kelvin  = "K"
    | base == Ampere  = "A"

-- | Used for conversion string input from user intended to be of the form
-- <Double> <unitprefix> <unitbase> to <desiredprefix>. Function pulls out 
-- the <Double> value by dropping anything that isn't a digit, then taking
-- the digits, then reading them as a Double
parseDouble :: String -> Double
parseDouble = read . takeWhile isDigit . dropWhile (not . isDigit)

-- | Grab the Rational power value out of the 3-ple
parsePower :: String -> Rational
parsePower prefix = second (findByPrefix prefix)

-- | Given a measurement and a prefix, convert to another measurement
convert :: Measurement -> Prefix -> Measurement
convert (MetricMeasurement x unit power) prefix 
    = MetricMeasurement (x * fromRational (power / b)) (M prefix (unitBase unit) c) (power / b)
    where
    (a, b, c) = findByPrefix prefix

-- | Gives the string form of a measurement with its unit abbreviation
reportMeasurement :: Measurement -> String
reportMeasurement (MetricMeasurement x unit power) 
    = show x ++ unitAbbr unit ++ getBaseAbbr (unitBase unit)

-- | Gives the full string form of a measurement with its full prefix and base name
-- Sort of like a verbose -v option
reportMeasurement' :: Measurement -> String
reportMeasurement' (MetricMeasurement x unit power)
    | x == 1 = show x ++ " " ++ unitStr unit
    | x == (-1) = show x ++ " " ++ unitStr unit
    | otherwise = show x ++ " " ++ unitStr unit ++ "s"

-- | The following functions are for getting data out of a Unit
unitStr :: Unit -> String
unitStr u@(M prefix base abbr) = prefix ++ lowerUnit u

-- | Lowers the first character of an enum type of Unit converted to a string.
-- Therefore Meter -> meter
lowerUnit :: Unit -> String
lowerUnit (M prefix base abbr) = map toLower (show base)

unitAbbr :: Unit -> String
unitAbbr u@(M prefix base abbr) = abbr

unitBase :: Unit -> MetricBaseUnit
unitBase (M prefix base abbr) = base

-- | Pull value out if its right or nothing if left
getRight :: Either t a -> Maybe a
getRight (Left  _) = Nothing
getRight (Right x) = Just x

-- | This module provides definitions for Measurement System Units and ways to parse
-- text into data so that it can be converted and then convert it back to Strings for
-- output. The Ry-RC bot uses this module to do the heavy lifting for unit conversions
module Data.Units where

import Data.List
import Data.Char
import Data.Maybe
import Control.Applicative

type Prefix = String
type Power  = String

type PrefixError = String

-- | The definitions for this list of String are taken from the
-- Pint unit conversion library for the Python programming language.
-- Check them out on <https://github.com/hgrecco/pint GitHub>.
definitions :: [String]
definitions = 
  ["yocto- = 1e-24 = y-"
  ,"zepto- = 1e-21 = z-"
  ,"atto- =  1e-18 = a-"
  ,"femto- = 1e-15 = f-"
  ,"pico- =  1e-12 = p-"
  ,"nano- =  1e-9  = n-"
  ,"micro- = 1e-6  = u-"
  ,"milli- = 1e-3  = m-"
  ,"centi- = 1e-2  = c-"
  ,"deci- =  1e-1  = d-"
  ,"deca- =  1e+1  = da-"
  ,"hecto- = 1e2   = h-"
  ,"kilo- =  1e3   = k-"
  ,"mega- =  1e6   = M-"
  ,"giga- =  1e9   = G-"
  ,"tera- =  1e12  = T-"
  ,"peta- =  1e15  = P-"
  ,"exa- =   1e18  = E-"
  ,"zetta- = 1e21  = Z-"
  ,"yotta- = 1e24  = Y-"

    -- binary_prefixes
  ,"kibi- = 2**10 = Ki-"
  ,"mebi- = 2**20 = Mi-"
  ,"gibi- = 2**30 = Gi-"
  ,"tebi- = 2**40 = Ti-"
  ,"pebi- = 2**50 = Pi-"
  ,"exbi- = 2**60 = Ei-"
  ,"zebi- = 2**70 = Zi-"
  ,"yobi- = 2**80 = Yi-"

    -- base_prefixes
  ,"met-  = 1e0   =   -"
  ,"lit-  = 1e0   =   -"
  ,"gra-  = 1e0   =   -"
  ,"can-  = 1e0   =   -"
  ,"amp-  = 1e0   =   -"
  ,"mol-  = 1e0   =   -"
  ,"kel-  = 1e0   =   -"
  ,"sec-  = 1e0   =   -"
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

-- | 'Unit' provides a wrapper for base units to include an appropriate prefix
-- for example "milli" in front of a Meter
data Unit = M Prefix MetricBaseUnit

-- | 'Measurement' Wraps a double value to be associated with a unit
data Measurement = MetricMeasurement Double Unit

-- | Function grabs the prefix of an input string intended to be of the form
-- <Double> <currentunit> to <desiredunit>. Function pulls out the prefix of <desiredunit>
getPrefix :: 
  String {- ^ input string -} ->
  Either PrefixError Prefix {- ^ prefix associated with input -}
getPrefix str = case parsePrefix $ take 5 $ reverse $ takeWhile (/= ' ') $ reverse str of 
                Left msg -> Left msg
                Right (a, b) -> Right a

strToMeasurement :: String -> Measurement
strToMeasurement str = MetricMeasurement (parseValue str) (parseUnit str)

-- | Used for conversion string input from user intended to be of the form
-- <Double> <currentunit> to <desiredunit>. Function pulls out the <currentunit>
-- and puts it in the Unit data constructor.
parseUnit :: String -> Unit
parseUnit str = M prefix (parseBase str)
    where prefix =  case parsePrefix $ getPrefixLine (take 3 $ dropWhile (`notElem` ['a'..'z']) str) definitions of
                            Left msg -> msg
                            Right (a, b) -> a

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

-- | Used for conversion string input from user intended to be of the form
-- <Double> <currentunit> to <desiredunit>. Function pulls out the <Double> value
-- by dropping anything that isn't a digit, then taking the digits, then reading them
-- as a Double
parseValue :: String -> Double
parseValue = read . takeWhile isDigit . dropWhile (not . isDigit)

-- | Takes a prefix string and the definitions list and parses through trying to match
-- a definition and returning a single space as the prefix if none could be found
getPrefixLine :: Prefix -> [String] -> String
getPrefixLine str []      = " " -- if it is not found then it is the base unit with no prefix
getPrefixLine str (x:xs)
    | str `isPrefixOf` x  = x
    | otherwise           = getPrefixLine str xs

-- | parseAbbr takes a definitions string where the end of the string holds the unit
-- abbreviation. It reverses the string to get to the end and parses until it hits
-- the first space and returns the abbreviation
parseAbbr ::
  String {- ^ from definitions list that has had prefix dropped -} ->
  String {- ^ as defined in definitions list -}
parseAbbr str = drop 1 $ takeWhile (/= ' ') (reverse str)

-- | Function parses a string and returns the Prefix and Prefix Abbr associated if there
-- are defintions in the definitions string list
parsePrefix :: 
  String {- ^ Representing a unit definition -} ->
  Either PrefixError (Prefix, String)
parsePrefix str
    | str `isPrefixOf` x  = Right (takeWhile (/= '-') x, parseAbbr (drop (length str) x))
    | otherwise           = Left "Prefix could not be found"
        where
        x = getPrefixLine str definitions


-- | Helper function to print a units prefix and base unit
unitStr :: Unit -> String
unitStr u@(M prefix base) = prefix ++ lowerUnit u

-- | Lowers the first character of an enum type of Unit converted to a string.
-- Therefore Meter -> meter
lowerUnit :: Unit -> String
lowerUnit (M prefix base) = map toLower (show base)

-- | Helper function to print the abbreviation of a Unit
unitAbbr :: Unit -> String
unitAbbr u@(M prefix base) = case (parsePrefix prefix) of
                             Left msg -> msg
                             Right (a, b) -> b ++ [head (lowerUnit u)]

-- | Helper function to print the prefix of a unit
unitPrefix :: Unit -> Prefix
unitPrefix (M prefix base) = prefix

-- | Helper function to print the base of a unit
unitBase :: Unit -> MetricBaseUnit
unitBase (M prefix base) = base

-- | Parses the power from the defintions list to be used in conversions
parsePower :: 
  String {- ^ Unit Prefix to find the definition -} ->
  String {- ^ The base 10 power of the prefix -}
parsePower prefix = reverse $ dropWhile (== ' ') (reverse $ take 5 $ dropWhile (/= '1') line)
    where line = (getPrefixLine prefix definitions)

parsePower' :: String -> String
parsePower' prefix = takeWhile (/= ' ') $ dropWhile (not . isDigit) $ getPrefixLine prefix definitions

-- | Converts a given measurement to a new scale in the current measurement system
-- given the prefix you want to convert it to
convert :: Measurement -> Prefix -> Measurement
convert (MetricMeasurement x unit) prefix 
    = MetricMeasurement (x * ((read (parsePower $ unitPrefix unit) :: Double) / (read (parsePower prefix) :: Double))) (M prefix (unitBase unit))

-- | Gives the string form of a measurement with its unit abbreviation
reportMeasurement :: Measurement -> String
reportMeasurement (MetricMeasurement x unit) 
    = show x ++ unitAbbr unit

-- | Gives the full string form of a measurement with its full prefix and base name
-- Sort of like a verbose -v option
reportMeasurement' :: Measurement -> String
reportMeasurement' (MetricMeasurement x unit)
    = show x ++ [' '] ++ unitStr unit

getRight :: Either t a -> Maybe a
getRight (Left  _) = Nothing
getRight (Right x) = (Just x)

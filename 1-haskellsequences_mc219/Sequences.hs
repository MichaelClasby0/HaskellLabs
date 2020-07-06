module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
-- Returns first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 x y
  | x > y     = x
  | otherwise = y

maxOf3 :: Int -> Int -> Int -> Int
-- Returns the largest of three Ints
maxOf3 x y z = maxOf2 x (maxOf2 y z) 

isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit c = c `elem` ['0'..'9']

-- False otherwise
isAlpha :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- isAlpha c = error "TODO: implement isAlpha"
isAlpha c = c `elem` ['a'..'z']++['A'..'Z']

digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt c = ord c - ord '0'

toUpper :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.

-- Pre: the character is an English letter
toUpper c
  | ord c >= 97   = chr (ord c - 32)
  | otherwise = c

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n = a + fromIntegral n * d

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n = a * r ^ n

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n = (fromIntegral n + 1) * (a + d * fromIntegral n * 0.5)

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
  | r == 1    = a * fromIntegral (n + 1)
  | otherwise = a * frac
  where
    frac = num / dom
    num = 1 - r ^ (n + 1)
    dom = 1 - r

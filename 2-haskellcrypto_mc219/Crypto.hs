module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption
{-
gcd :: Int -> Int -> Int
gcd m n
  | n == 0    = m
  | otherwise = gcd n (m `mod` n)

During testing I discovered that gcd of negative numbers under the above
definition had some weird properties, after some research I redefined the function
below to match with the built in functions outputs
-}

gcd :: Int -> Int -> Int
gcd m n = gcd' (abs m)  (abs n)
  where
    gcd' m n
      | n == 0    = m
      | otherwise = gcd n (m `mod` n)


phi :: Int -> Int
phi m = length [a | a <- [1..m], gcd a m == 1]

-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
-- au+bv = gcd(a, b) = d
-- (v′, (u′ − qv′)) are Bezout coefficients of a and b respectively.
computeCoeffs :: Int -> Int -> (Int, Int)
-- Pre: a,b >= 0
computeCoeffs a b
  | b == 0     = (1, 0)
  | otherwise  = (v', u' - v' * q)
    where
      (u', v') = computeCoeffs b r
      (q, r)   = quotRem a b

-- Inverse of a modulo m
-- Pre: gcd(a, m) = 1
inverse :: Int -> Int -> Int
inverse a m = u `mod` m
  where
    (u, _)  = computeCoeffs a m

-- Calculates (a^k mod m)
modPow :: Int -> Int -> Int -> Int
-- Pre: 0 <= a < m
-- modPow a k m = (a ^ k) `mod` m
modPow a k m
  | k == 0 = a ^ k `mod` m
  | odd k  = (a * (modPow (a^2 `mod` m) (k `div` 2) m)) `mod` m
  | even k = modPow (a^2 `mod` m) (k `div` 2) m

-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
-- Pre: a /= 0, b > 1
smallestCoPrimeOf a
  = smallestCoPrimeOf' a 2
  where
    smallestCoPrimeOf' :: Int -> Int -> Int
    smallestCoPrimeOf' a b
      | gcd a b == 1 = b
      | otherwise    = smallestCoPrimeOf' a (b + 1)

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
-- l = phi q * phi p
-- since p and q are both prime phi p = p - 1 and phi q = q - 1
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q = ((e, n), (d, n))
  where
    n = p * q
    l = (p - 1) * (q - 1)
    e = smallestCoPrimeOf l
    d = inverse e l


-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e, n) = modPow x e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d, n) = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt c = ord c - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
toChar x = chr (x + ord 'a')

-- Length of the alphabet
alphaLen :: Int
alphaLen = 26

-- "adds" two letters
add :: Char -> Char -> Char
add c c' = toChar ((toInt c + toInt c') `mod` alphaLen)

-- "substracts" two letters
substract :: Char -> Char -> Char
substract c c' = toChar ((toInt c - toInt c') `mod` alphaLen)


-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt key plain = map (add key) (plain)

-- c - k required therefore input for substract function must be flipped
ecbDecrypt :: Char -> String -> String
ecbDecrypt key cipher = map (flip substract key) cipher

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string

-- Pre: Plain text must be a string of lowercase English characters
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt key initVec plain = cbcEncrypt' initVec plain
  where
    cbcEncrypt' prevEncrypt currentStr
      | currentStr == "" = ""
      | otherwise        =  next : cbcEncrypt' next ps
        where
          next = key `add` (p `add` prevEncrypt)
          p:ps = currentStr


cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt key initVec cipher = cbcDecrypt' initVec cipher
  where
    cbcDecrypt' prevChar currentStr
      | currentStr == "" = ""
      | otherwise        = next : cbcDecrypt' c cs
        where
          next = (c `substract` key) `substract` prevChar
          c:cs = currentStr

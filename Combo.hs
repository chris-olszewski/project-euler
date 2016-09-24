module Combo
( factorial
, permutations
, binomial
, frequency
) where

import Data.List hiding (permutations)

factorial :: (Integral a) => a -> a
factorial x = factorialHelper 1 x

factorialHelper :: (Integral a) => a -> a -> a
factorialHelper count 0 = count
factorialHelper count x = factorialHelper (count * x) (x - 1)

binomial :: (Integral a) => a -> a -> a
binomial a b = (factorial a) `div` ((factorial b) * (factorial (a - b)))

permutations :: (Integral a) => a -> a -> a
permutations n r = (factorial n) `div` (factorial (n - r))

frequency :: (Eq a) => [a] -> [(a, Int)]
frequency xs = freqHelper xs (zip (nub xs) (repeat 0))

freqHelper :: (Eq a) => [a] -> [(a, Int)] -> [(a, Int)]
freqHelper [] freq = freq
freqHelper (x:xs) freq =
                let newFreq = map (incrementFreq x) freq
                in freqHelper xs newFreq

incrementFreq :: (Eq a) => a -> (a, Int) -> (a, Int)
incrementFreq x (y, f)
    | x == y = (y, f + 1)
    | otherwise = (y, f)

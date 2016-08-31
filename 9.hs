-- Problem find Pythagorean triplet s.t. a + b + c = 1000 find a*b*c
import Data.List

pythagoreanTriplets = [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a*a + b*b == c*c, and [a < b, b < c]]

tripletSum :: (Num a) => (a,a,a) -> a
tripletSum (a,b,c) = a + b + c

tripletProduct :: (Num a) => (a,a,a) -> a
tripletProduct (a,b,c) = a*b*c

answer = tripletProduct $ head $ dropWhile ((/= 1000) . tripletSum) pythagoreanTriplets

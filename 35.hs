import Data.Char (digitToInt)
import Prime

rotations :: [a] -> [[a]]
rotations xs = take (length xs) $ iterate rotate xs

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ x:[]

intToDigits :: Int -> [Int]
intToDigits = map digitToInt . show

digitsToInt :: [Int] -> Int
digitsToInt = read . concat . map show

circularPrimes x = [ n | n <- (takeWhile (<x) primes), all test (((map digitsToInt) . rotations . intToDigits) n) ]

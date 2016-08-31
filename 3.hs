-- What is the largest prime factor of the number 600851475143

import Prime
import Data.List

-- First attempt
primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n
    | test n = [n]
    | otherwise = divisor : (primeFactors $ div n divisor)
        where divisor = head [x | x <- [n,n-1..1],  mod n x == 0, test x]

-- Second attempt. Far slower
primeFactors' :: (Integral a) => a -> [a]
primeFactors' n = [x | x <- [n,n-1..1], mod n x == 0, test x]

main = putStrLn $ show $ head $ primeFactors' 600851475143

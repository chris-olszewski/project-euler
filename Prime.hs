module Prime
( test
, primes
, divisors
) where
import Data.List

test :: Int -> Bool
test n
    | n < 2 = False
    | otherwise = and $ map ((/= 0) . (mod n)) $ takeWhile ((<= n) . (^2)) primes

primes = 2:[ n | n <- [3,5..], test n]

primeFactorization :: Int -> [Int]
primeFactorization 1 = []
primeFactorization n = primeFactorization (n `div` primeDivisor) ++ primeDivisor:[]
                        where primeDivisor = head [ x | x <- primes, n `mod` x == 0 ]

-- I assume this is very slow
divisors :: Int -> [Int]
divisors = init . nub . (map product) . subsequences . primeFactorization

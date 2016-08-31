-- Find 10001st prime number

import Prime
import Data.List

primes = [x | x <- [2..], test x]

nthPrime n = head $ drop (n-1) primes

main =
    putStrLn $ show $ nthPrime 10001

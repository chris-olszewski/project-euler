-- Find the sum of all the primes below two million
import Prime

answer = sum [ x | x <- [2..(floor 2e6)], test x]

main =
    putStrLn $ show answer

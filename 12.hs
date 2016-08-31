-- What is the value of the first triangle number to have over five hundered divisors?

import Data.List
import Prime

triangleNumbers = [ sum [0..x] | x <- [1..]]

answer = head $ dropWhile ((<500) . length . divisors) triangleNumbers

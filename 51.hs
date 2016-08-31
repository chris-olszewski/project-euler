import Prime
import Data.List

replace :: Char -> Char -> String -> String
replace x y s = map (\n -> if n == x then y else n) s

valueFamily :: String -> [Int]
valueFamily s = map (\n -> read (replace '*' n s)) ['0'..'9']

primeFamily :: String -> [Int]
primeFamily = (filter test) . nub . valueFamily

replaceAt :: String -> Char -> Int -> String
replaceAt s c x = take x s ++ [c] ++ drop (x + 1) s

templates :: String -> [String]
templates s = nub $ map (flip ((flip replace) '*') s) ['0'..'9']

-- This is extremely jank, for some reason 2nd num in list is correct answer.
eightPrimeFamily = [x | x <- primes, any (== 8)  (map (length . primeFamily) (templates $ show x))]

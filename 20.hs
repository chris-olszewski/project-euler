factorial :: Integer -> Integer
factorial n = factorialHelper n 1

factorialHelper :: Integer -> Integer -> Integer
factorialHelper 0 counter = counter
factorialHelper n counter = factorialHelper (n - 1) (n * counter)

sumDigits :: Integer -> Integer
sumDigits = sum . map (read . (:[])) . show

answer = sumDigits $ factorial 100

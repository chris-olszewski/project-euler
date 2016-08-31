digitSum :: Integer -> Integer
digitSum = sum . map (read . (:[])) . show

answer = digitSum $ 2^1000

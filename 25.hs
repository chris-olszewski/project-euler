-- Find the index of the first term in the Fibonacci sequence to contain 1000 digits?
fib = 1:1:(zipWith (+) fib $ tail fib)

indexedFib = zip [1..] fib

answer = fst $ head $ dropWhile ((< 1000) . length . show . snd) indexedFib

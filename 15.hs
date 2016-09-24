import Combo (binomial)
-- right, but very bad
numPaths :: Int -> Int -> Int
numPaths x y
    | x == 0 || y == 0 = 1
    | x == y = 2 * (numPaths (x - 1) y)
    | otherwise = (numPaths (x - 1) y) + (numPaths x (y - 1))

-- because essentially they need to choose 20 paths from 40 possible choices.
answer = binomial 40 20

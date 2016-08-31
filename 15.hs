numPaths :: Int -> Int -> Int
numPaths x y
    | x == 0 || y == 0 = 1
    | x == y = 2 * (numPaths (x - 1) y)
    | otherwise = (numPaths (x - 1) y) + (numPaths x (y - 1))


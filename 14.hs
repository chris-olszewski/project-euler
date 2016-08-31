import Data.List

collatzSequence :: Int -> [Int]
collatzSequence n = takeWhile (/= 1) $ iterate collatzHelper n

collatzHelper :: Int -> Int
collatzHelper n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3 * n + 1

collatz :: Int -> (Int, Int)
collatz n = (n, length $ collatzSequence n)

tupleMaximum :: (Ord a) => (a,a) -> (a,a) -> (a,a)
tupleMaximum (x1,y1) (x2,y2)
    | y1 > y2 = (x1,y1)
    | otherwise = (x2,y2)

answer = foldl1 tupleMaximum $ map collatz [1..(floor 1e6)]

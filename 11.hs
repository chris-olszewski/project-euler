-- What is the greatest product of four adjacent numbers in the same direction (up,down,left,right, or diagonally) in the 20x20 grid?

import Data.List

main = do
    gridString <- getContents
    putStrLn $ greatestProduct $ (read2d . gridFromString) gridString

gridFromString :: (Num a) => String -> [[a]]
gridFromString s = map words $ lines s

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (\xs -> (map f xs))

read2d :: (Read a) => [[String]] -> [[a]]
read2d = map2d read

groupsOf :: Int -> [a] -> [[a]]
groupsOf s xs = map (\x -> take s (drop x xs)) [0..(length xs) - s]

greatestProduct :: (Num a) => [[a]] -> a
greatestProduct xxs = maximum $ products xxs

products :: (Num a) => [[a]] -> [a]
products xxs = (up xxs) ++ (left xxs) ++ (forwardDiag xxs) ++ (backwardDiag xxs)

left :: (Num a) => [[a]] -> [a]
left xxs = map maximum $ map2d product $ map (groupsOf 4) xxs

up :: (Num a) => [[a]] -> [a]
up = left . transpose

forwardDiag :: (Num a) => [[a]] -> [a]
forwardDiag xxs = 

diagGroupOf :: Int -> [[a]] -> (Int,Int) -> Maybe [a]

backwardDiag :: (Num a) => [[a]] -> [a]


main = do
    filetext <- readFile "13.in"
    let answer = sum $ map read $ lines filetext
    putStrLn $ take 10 $ show answer

squareOfSum :: [Int] -> Int
squareOfSum = (^2) . (foldl (+) 0)


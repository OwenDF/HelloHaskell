filter2 f (x:xs)
  | (x:xs) == [] = []
  | otherwise = if f x
    then x : (filter2 f xs)
    else filter2 f xs

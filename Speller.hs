spellOut word =
  [word!!0] ++ " is for " ++ word

appendString accumulator words = case words of
  [x] -> accumulator++", and "++(spellOut x)
  (x:xs) -> appendString (accumulator++", "++(spellOut x)) xs

speller [] = do return()

speller (firstWord:words) = do
  putStrLn (appendString (spellOut firstWord) words)

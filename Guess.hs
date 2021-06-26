check word display c = (elem c word, [if x==c then c else y | (x,y) <- zip word display])

mkguess word display n =
  do putStrLn (display++" "++ take n (repeat '*'))
     putStr " Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

turn word display n =
  do if n==0
    then putStrLn "You Lose"
    else if word==display
      then putStrLn "You Win!"
      else mkguess word display n

starman word n = turn word ['-' | x <- word] n

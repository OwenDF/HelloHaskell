import ShowParser (parseShow)
import Person

rec_str = show [person1,person2]
main = putStrLn $ parseShow rec_str

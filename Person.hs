module Person where

data Person = MkPerson {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]
} deriving (Show)

data Address = MkAddress {
  line1 :: String,
  number :: Integer,
  street :: String,
  town :: String,
  postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

myHouse = MkAddress "16 Futura House" 168 "Grange Road" "London" "SE1 3BN"

person1 = MkPerson
  "Owen Daly-Furlong"
  myHouse
  1
  [Red, Blue]

person2 = MkPerson
  "Charles Slater"
  myHouse
  2
  [Yellow]

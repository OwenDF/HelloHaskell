class Colour a where
  dark :: a -> Bool
  lighten :: a -> a

data Bright = Blue | Red deriving (Show, Read)
data Pastel = Turqoise | Tan deriving (Show, Read)
instance Colour Bright where
  dark = darkBright
  lighten = lightenBright
instance Colour Pastel where
  dark = darkPastel
  lighten = lightenPastel


darkBright Blue = True
darkBright Red  = False
lightenBright Blue = Red
lightenBright Red = Red
darkPastel Turqoise = True
darkPastel Tan = False
lightenPastel Turqoise = Tan
lightenPastel Tan = Tan

data Foo = Bar | Baz


instance Show Foo where
  show Bar = "It is a bar"
  show Baz = "It is a baz"


data Foo2 = Bar2 | Baz2 deriving (Read, Show)

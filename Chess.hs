

data Character = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Show)

data Affiliation = North | East | South | West
                   deriving (Show)

data Location = Location Int Int
                deriving (Show)

data Piece = Piece Affiliation Character Location
             deriving (Show)



data Board = Board Int Int [Piece]
             deriving (Show)

boardSize (Board l h _) = l * h

asciiBoard (Board l h ps) = [ Location x y | x <- [0..(l-1)], y <- [0..(h-1)] ]


main = do
          let b = Board 8 8 [ Piece South Pawn (Location 0 1) ]
          putStrLn $ show $ boardSize b
          putStrLn $ show $ asciiBoard b

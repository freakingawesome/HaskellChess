import qualified Data.Map as Map

data Character = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Show)

data Affiliation = North | East | South | West
                   deriving (Show)

type Location = (Int,Int)

data Move = Move Location Location Piece
            deriving (Show)

data Piece = Piece Affiliation Character [Move]
             deriving (Show)

data Square = Empty | Square Piece
              deriving (Show)

data Board = Board (Map.Map Location Square) 
             deriving (Show)

emptyBoard w h = Board (Map.fromList [ ((x,y), Empty) | x <- [0..(w-1)], y <- [0..(h-1)] ])

--boardSize (Board l h _) = l * h

--asciiBoard (Board l h ps) = [ Location x y | x <- [0..(l-1)], y <- [0..(h-1)] ]


--main = do
          --let b = emptyBoard 8 8 
          --let c = Map.lookup (2,4) b
          --putStrLn $ show $ b
--          putStrLn $ show $ asciiBoard b

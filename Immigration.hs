{-
 - Comonad implementation of Immigration Game.
 -}

module Immigration where

import Control.Comonad (Comonad(..), (=>>))
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)

import Universe

-- Basic Immigration Game

-- Unlike Conway's Game, cells in the Immigration Game can take on 3 values
data Cell = Dead | Red | Blue
    deriving (Eq, Show)

-- Succession rules for the immigration game
immigrationRule :: Universe2D Cell -> Cell
immigrationRule u
    | length aliveNeighbors == 2           = extract u
    | length aliveNeighbors == 3 && twoRed = Red
    | length aliveNeighbors == 3           = Blue
    | otherwise                = Dead
  where aliveNeighbors = filter (/= Dead) (neighbors u)
        twoRed         = length (filter (== Red) aliveNeighbors) >= 2

renderBoard :: (Int, Int) -> (Int, Int) -> Universe2D Cell -> String
renderBoard x y = unlines . map (concatMap renderCell) . takeRange2D x y
  where renderCell Red  = "1"
        renderCell Blue = "2"
        renderCell Dead = "."

-- Run 100 iterations and print the intermediate steps
gameLoop :: Int -> Universe2D Cell -> IO (Universe2D Cell)
gameLoop n u = do
    threadDelay (10 ^ 6)
    putStrLn "" >> print n
    putStr $ renderBoard (-20, -20) (20, 20) u
    if n <= 1
        then return u
        else gameLoop (n-1) (u =>> immigrationRule)


-- Immigration game as a 2-player game:
-- Whichever player has the most cells of their color in then 8x8 rectangle in the
-- middle of the infinite grid after 100 turns is declared the winner.

data PlayerCell = Empty | Full
    deriving (Show, Eq, Ord)

swap :: PlayerCell -> PlayerCell
swap Empty = Full
swap Full  = Empty

-- A player input is a 4x4 grid of PlayerCells
type Input = [[PlayerCell]]

-- opposite of concat for Inputs
cut :: [PlayerCell] -> Input
cut l = [take 4 l,          take 4 $ drop 4  l,
         take 4 $ drop 8 l, take 4 $ drop 12 l]

-- Replace one value in an Input using its index
swapAt :: Int -> Input -> Input
swapAt i = cut . (\l -> take i l ++ (swap (l!!i) : drop (i+1) l)) . concat

-- Render an Input for printing
pprint :: Input -> String
pprint = unlines . (map . map) (\x -> if x == Full then 'X' else '.')

-- The default Input is a blank grid
blankPlayer :: Input
blankPlayer = replicate 4 $ replicate 4 Empty

-- Convert this list representation to the comonadic representation
setup :: Input -> Input -> Universe2D Cell
setup p1 p2 = fromList2D Dead (player1 ++ player2)
  where player1 = replace Red p1
        player2 = replace Blue $ reverse p2
        replace color = (map . map) (\x -> if x == Full then color else Dead)

-- Compute scores after a game
scores :: Universe2D Cell -> [(Cell, Int)]
scores u = [pair Red u, pair Blue u]
  where count c  = length . filter (== c) . concat . takeRange2D (-8, -8) (8, 8)
        pair color x = (color, count color x)

gameLength :: Int
gameLength = 100

-- Run 100 iterations and return the result
runGame :: Input -> Input -> (Int, Int)
runGame p = tabulate . scores . run gameLength . setup p
  where run n u = if n <= 1 then u else run (n-1) (u =>> immigrationRule)
        tabulate r = (fromMaybe 0 $ lookup Red r, fromMaybe 0 $ lookup Blue r)


-- Example: Gosper glider vs f-pentomino

-- A player's entry is a 4x8 grid
player1 = [ [Empty, Full,  Empty, Empty]
          , [Full,  Empty, Empty, Empty]
          , [Full,  Full,  Full,  Empty]
          , [Empty, Empty, Empty, Empty] ]

player2 = [ [Empty, Empty, Empty, Empty]
          , [Empty, Full,  Full,  Empty]
          , [Full,  Full,  Empty, Empty]
          , [Empty, Full,  Empty, Empty] ]

example :: IO ()
example = do
    game <- gameLoop gameLength $ setup player1 player2
    print $ scores game

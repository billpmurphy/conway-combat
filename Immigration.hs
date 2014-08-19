{-
 - Comonad implementation of Immigration Game.
 -}

import Control.Comonad (Comonad(..), (=>>))
import Control.Concurrent (threadDelay)

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
    putStrLn ""
    print n
    putStr $ renderBoard (-20, -20) (20, 20) u
    if n <= 1
        then return u
        else gameLoop (n-1) (u =>> immigrationRule)


-- Immigration game as a 2-player game:
-- Whichever player has the most cells of their color in then 8x8 rectangle in the
-- middle of the infinite grid after 100 turns is declared the winner.

data PlayerCell = Empty | Full
    deriving (Show, Eq)

setup :: [[PlayerCell]] -> [[PlayerCell]] -> Universe2D Cell
setup p1 p2 = fromList2D Dead (player1 ++ player2)
  where player1 = replace Red  . take 4 . map (take 4) $         p1
        player2 = replace Blue . take 4 . map (take 4) $ reverse p2
        replace color = (map . map) (\x -> if x == Full then color else Dead)

tabulate :: Universe2D Cell -> [(Cell, Int)]
tabulate u = [pair Red u, pair Blue u]
  where count c  = length . filter (== c) . concat . takeRange2D (-8, -8) (8, 8)
        pair c x = (c, count c x)

-- Run 100 iterations and return the result
runGame :: [[PlayerCell]] -> [[PlayerCell]] -> [(Cell, Int)]
runGame p = tabulate . run 100 . setup p
  where run n u = if n <= 1 then u else run (n-1) (u =>> immigrationRule)

-- Example: Gosper glider vs f-pentomino

-- A player's entry is a 4x8 grid
player1 = [ [Empty, Full,  Empty]
          , [Full,  Empty, Empty]
          , [Full,  Full,  Full]
          , [Empty, Empty, Empty] ]

player2 = [ [Empty, Empty, Empty]
          , [Empty, Full,  Full ]
          , [Full,  Full,  Empty]
          , [Empty, Full,  Empty] ]

main :: IO ()
main = print $ runGame player1 player2
--    game <- gameLoop 100 $ setup player1 player2
--    print $ tabulate game

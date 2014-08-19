Conway Combat: A 2-player cellular automata game
------------------------------------------------

The repo implements the [Immigration
Game](http://www.conwaylife.com/wiki/index.php?title=Immigration) using
comonads. The Immigration Game can be played as a 2-player game, in which
players compete to control territory by spreading their own colored cells while
also trying to block their opponent from doing the same.

## So what is this, exactly? ##

Read [this](http://kukuruku.co/hub/haskell/cellular-automata-using-comonads).
Got all that? Good.

To implement the Immigration Game (rather than the vanilla Conway's Game) we
need cells that can take on 3 values and rules for modifying them:

```haskell
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
```

We also need to change our simple rendering function and game loop:

```haskell
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
```

We can also adapt the Immigration Game as a two-player game: Whichever player
has the most cells of their color in then 8x8 rectangle in the middle of the
infinite grid after 100 turns is declared the winner. That is, your score at
the end of the round is the number of your colored tiles present in the center
8x8 rectangle:

```haskell
scores :: Universe2D Cell -> [(Cell, Int)]
scores u = [pair Red u, pair Blue u]
  where count c  = length . filter (== c) . concat . takeRange2D (-8, -8) (8, 8)
        pair color x = (color, count color x)
```

Check out the `Universe.hs` file for the comonad cellular automata grid
(heavily based on
[this](http://www.conwaylife.com/wiki/index.php?title=Immigration)) and the
`Immigration.hs` file for the implementation of the 2-player territory control
game. See `Solver.hs` for a genetic algorithms-based method to find better
starting positions for the game.

Conway Combat: A 2-player cellular automata game
------------------------------------------------

The repo implements the [Immigration
Game](http://www.conwaylife.com/wiki/index.php?title=Immigration) using
comonads. The Immigration Game can be played as a 2-player game, in which
players compete to control territory by spreading their own colored cells while
also trying to block their opponent from spreading their own colored cells.

## What is this, exactly? ##

Read [this](http://kukuruku.co/hub/haskell/cellular-automata-using-comonads). Got it? Good.

To implement the Immigration Game, we need cells that can take on 3 values and
rules for modifying them:

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

We can adapt the Immigration Game as a two-player game: Whichever player has
the most cells of their color in then 8x8 rectangle in the middle of the
infinite grid after 100 turns is declared the winner.


Check out the `Universe.hs` file for the comonad cellular automata grid
(heavily based on
[this](http://www.conwaylife.com/wiki/index.php?title=Immigration)) and the
`Immigration.hs` file for the implementation of the 2-player territory control
game.

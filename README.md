Conway Combat: A 2-player cellular automata game
------------------------------------------------

Status: Work in progress


The repo implements the [Immigration
Game](http://www.conwaylife.com/wiki/index.php?title=Immigration) using
comonads (based on [this excellent
article](http://kukuruku.co/hub/haskell/cellular-automata-using-comonads) that
shows how cellular automata can be represented as comonads). The Immigration
Game can be played as a 2-player game, in which players compete to control
territory by spreading their own colored cells while also trying to block their
opponent from spreading their own colored cells.

Check out the `Universe.hs` file for the comonad implementation of an infinite
cellular automata grid (heavily based on
[this](http://www.conwaylife.com/wiki/index.php?title=Immigration)) and the
`Immigration.hs` file for the implementation of the Immigration Game as a
2-player territory control game.

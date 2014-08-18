{-
 - Comonad implementation of an infinite cellular automata grid, adapted with
 - minor modifications from Kukuruku's implementation here:
 - http://kukuruku.co/hub/haskell/cellular-automata-using-comonads
 -
 -}

module Universe where

import Control.Applicative (pure)
import Control.Comonad (Comonad(..))


-- Universe: a comonad/zipper infinite list
data Universe a = Universe [a] a [a]

-- Shift the zipper one element to the left or right
left, right :: Universe a -> Universe a
left  (Universe (a:as) x bs) = Universe as a (x:bs)
right (Universe as x (b:bs)) = Universe (x:as) b bs

-- Create a new Universe given functions to create the infinite lists
makeUniverse :: (a -> a) -> (a -> a) -> a -> Universe a
makeUniverse fl fr x = Universe (tail $ iterate fl x) x (tail $ iterate fr x)

-- Build a Universe using a finite list and a default value
fromList :: a -> [a] -> Universe a
fromList def (x:xs) = Universe (repeat def) x (xs ++ repeat def)

instance Functor Universe where
    fmap f (Universe as x bs) = Universe (fmap f as) (f x) (fmap f bs)

instance Comonad Universe where
    duplicate = makeUniverse left right
    extract (Universe _ x _) = x

-- Get the center item and the two adjacent items
nearest3 :: Universe a -> [a]
nearest3 u = map extract [left u, u, right u]

-- Return a range from the zipper using the lower and upper indicies, inclusive.
-- e.g., takeRange (-10, 10) will return the center element and 10 elements
-- from each side
takeRange :: (Int, Int) -> Universe a -> [a]
takeRange (a, b) u = take (b-a+1) x
    where Universe _ _ x
            | a < 0 = iterate left u !! (-a + 1)
            | otherwise = iterate right u !! (a - 1)


-- Two-dimensional comonad/zipper
newtype Universe2D a = Universe2D { getUniverse2D :: Universe (Universe a) }

instance Functor Universe2D where
    fmap f = Universe2D . (fmap . fmap) f . getUniverse2D

instance Comonad Universe2D where
    extract = extract . extract . getUniverse2D
    duplicate = fmap Universe2D . Universe2D . shifted . shifted . getUniverse2D
      where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
            shifted = makeUniverse (fmap left) (fmap right)

-- Build a Universe2D given a list of lists and a default value
fromList2D :: a -> [[a]] -> Universe2D a
fromList2D def = Universe2D . fromList udef . fmap (fromList def)
  where udef = Universe (repeat def) def (repeat def)

-- Return a rectangle from the 2D zipper using a bounding box, inclusive.
takeRange2D :: (Int, Int) -> (Int, Int) -> Universe2D a -> [[a]]
takeRange2D (x0, y0) (x1, y1)
    = takeRange (y0, y1) . fmap (takeRange (x0, x1)) . getUniverse2D

-- Get the 8 cells surrounding the center cell in a Universe2D
neighbors :: (Universe2D a) -> [a]
neighbors u =
    [ nearest3 . extract . left                 -- 3 cells in row above
    , pure     . extract . left  . extract      -- cell to the left
    , pure     . extract . right . extract      -- cell to the right
    , nearest3 . extract . right                -- 3 cells in row below
    ] >>= ($ getUniverse2D u)

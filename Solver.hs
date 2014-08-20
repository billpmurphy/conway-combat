{-# LANGUAGE FlexibleInstances #-}

{- Find optimal starting positions for the 2-player Immigration Game using
 - genetic algorithms. Obviously, since the game is implemented using
 - comonadic infinite lists, this isn't exactly efficient, but it is fun.
 -}

import Control.Monad (liftM, replicateM)
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.List (sortBy, sort, nub)
import qualified Data.Map as M (toList, Map(..), fromListWith)
import Data.Ord (comparing)

import Immigration

type MutFactor = Int
type Fitness = Int

class Gene a where
    mutate    :: (Gene a, MonadRandom m) => MutFactor -> a -> m a
    fitness   :: (Gene a, MonadRandom m) => [a] -> m (M.Map a Fitness)
    offspring :: (Gene a, MonadRandom m) => Int -> MutFactor -> a -> m [a]
    nextGen   :: (Gene a, MonadRandom m) => Int -> MutFactor -> [a] -> m [a]

instance Gene Input where
    mutate 0 p = return p
    mutate n p = do
        rand <- getRandomR (0, 15)
        mutate (n-1) $ swapAt rand p

    -- Play each configuration against each other to obtain their fitness values
    fitness = return . M.fromListWith (+) . rounds
      where round (a:b:_) = let g = runGame a b in [(a, fst g), (b, snd g)]
            rounds = concat . map round . matchups
            matchups = filter(\x -> nub x == x) . nub . map sort . replicateM 2

    -- Produce the offspring of a configuration by mutating it
    offspring children mut = sequence . map (mutate mut) . replicate children

    -- Produce the next generation, using the fitness scores from a round-robin
    nextGen par mut ins = do
        fit <- fitness ins
        let parents = map fst . take par . sortBy (comparing snd) $ M.toList fit
        next <- sequence $ map (offspring (length ins `quot` par) mut) parents
        return $ liftM concat next

-- First generation: n random configurations
firstGen :: MonadRandom m => Int -> m [Input]
firstGen n = sequence . map (mutate 500) $ replicate n blankPlayer


-- Simple example: iterations = 3, population = 3, mutations = 3, parents = 1
evolve :: Int -> [Input] -> IO ()
evolve n gen = do
    putStrLn $ "\nGen" ++ show n ++ ":"
    mapM_ (putStrLn . pprint) gen
    next <- nextGen 1 3 gen
    if n > 2
        then return ()
        else evolve (n+1) next

main :: IO ()
main = firstGen 3 >>= evolve 0

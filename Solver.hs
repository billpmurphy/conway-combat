{- Find optimal starting positions for the 2-player Immigration Game using
 - genetic algorithms. Obviously, since the game is implemented using
 - comonadic infinite lists, this isn't exactly efficient, but it is fun.
 -}

import Control.Monad (liftM, replicateM)
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.List (sortBy, sort, nub)
import Data.Map as M (toList, Map(..), fromListWith)
import Data.Ord (comparing)

import Immigration


-- Randomly mutate a configuration, flipping a certain number of bits
mutate:: MonadRandom m => Int -> Input -> m Input
mutate 0 p = return p
mutate n p = do
    rand <- getRandomR (0, 15)
    mutate (n-1) $ repl rand p
  where repl i = cut . (\l -> take i l ++ (swap (l!!i) : drop (i+1) l)) . concat
        cut  l = [take 4 l,          take 4 $ drop 4  l,
                  take 4 $ drop 8 l, take 4 $ drop 12 l]

-- Produce the offspring of a configuration by mutating it
offspring :: MonadRandom m => Int -> Int -> Input -> m [Input]
offspring children mut = sequence . map (mutate mut) . replicate children

-- First generation: n random configurations
firstGen :: MonadRandom m => Int -> m [Input]
firstGen n = sequence . map (mutate 500) $ replicate n blankPlayer

-- Play each configuration against each other to obtain their fitness values
getFitness :: [Input] -> M.Map Input Int
getFitness = M.fromListWith (+) . rounds
  where round (a:b:_) = let game = runGame a b in [(a, fst game), (b, snd game)]
        rounds = concat . map round . matchups
        matchups = filter(\x -> nub x == x) . nub . map sort . replicateM 2

-- Produce the next generation, using the fitness scores from a round-robin
nextGen :: MonadRandom m => Int -> Int -> [Input] -> m [Input]
nextGen par mut ins = liftM concat . sequence
                    . map (offspring (length ins `quot` par) mut)
                    . map fst . take par . sortBy (comparing snd) . M.toList
                    $ getFitness ins


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

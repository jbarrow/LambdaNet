module Network.Layer
( LayerDefinition(..)

, Connectivity
, RandomTransform

, connectFully
) where

import Network.Neuron
import Linear
import System.Random

data LayerDefinition a = LayerDefinition { neuron :: (Neuron a)
                                         , neuronCount :: Int
                                         , connect :: (Connectivity a)
                                         }

type RandomTransform a = [a] -> [a]
type Connectivity a = Int -> Int -> Matrix a

-- connectFully
--   a connectivity function that fully connects input neurons
--   to the output neurons
-- parameters
--   i is the count of input neurons for a layer
--   j is the count of the output neurons for a layer
-- returns
--   a i by j connectivity matrix
--   elements of matrix in {0, 1}
--   (or it could return boolean values)
connectFully :: Num a => Int -> Int -> Matrix a
connectFully i j = take i (repeat (take j (repeat 1)))

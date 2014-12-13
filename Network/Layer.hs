module Network.Layer
( LayerDefinition(..)

, Connectivity
, RandomTransform

, connectFully
, randomList
, boxMuller
, normals
, uniforms
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

-- Initialize an infinite random list list with:
randomList :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> [a]
randomList transform = transform . randoms

-- Define a transformation on the uniform distribution to generate
-- normally distributed numbers in Haskell (the Box-Muller transform)
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller x1 x2 = (z1, z2) where z1 = sqrt (log (-2) * x1) * cos (2 * pi * x2)
                                 z2 = sqrt (log (-2) * x1) * sin (2 * pi * x2)

-- Apply the Box-Muller transform
normals :: Floating a => [a] -> [a]
normals (x1:x2:xs) = z1:z2:(normals xs)
                        where (z1, z2) = boxMuller x1 x2

uniforms :: Floating a => [a] -> [a]
uniforms xs = xs

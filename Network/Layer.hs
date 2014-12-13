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

data LayerDefinition a = LayerDefinition (Neuron a) Int (Connectivity a)

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
randomList :: (RandomGen g, Random a, Floating a) => g -> [a]
randomList (normals|ts|uniforms) . randoms

-- Define a transformation on the uniform distribution to generate
-- normally distributed numbers in Haskell (the Box-Muller transform)
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller x1 x2 = (z1, z2) where z1 = sqrt . log x1 * cos (2 * pi * x2)
                                 z2 = sqrt . log x1 * sin (2 * pi * x2)

normals :: Floating a => [a] -> [a]
normals (x1:x2:us) = x1:x2:(boxMuller us)
                        where (x1, x2) = boxMuller x1 x2

uniforms :: Floating a => [a] -> [a]
uniforms us = us

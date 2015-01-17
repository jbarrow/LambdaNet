{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Network.SOM
( SOM(..)
, MapDefinition(..)

, randomNeuron
, makeVectors
, reshapeList
, distance
) where

import           Network.Layer
import           Network.Network
import           Network.Neuron

import           Numeric.LinearAlgebra
import           System.Random

data SOM = SOM { neuronMap :: [[Vector Double]] }

-- | A definitution type for the SOM, it contains the dimensions of each layer (x, y)
--   and the dimension of the input vector (dim)
data MapDefinition = MapDefinition { x   :: Int
                                   , y   :: Int
                                   , inputDim :: Int
                                   }

instance Network (SOM) where
  type Parameters = MapDefinition

  predict :: Vector Double -> SOM -> Vector Double
  predict inputs network = inputs

  createNetwork :: (RandomGen g) => RandomTransform -> g -> Parameters -> SOM
  createNetwork transformation g def = SOM randomVectors
    where randomVectors = reshapeList (x def) $
                          makeVectors transformation g (inputDim def)
                          ((x def) * (y def))

reshapeList :: Int -> [a] -> [[a]]
reshapeList x [] = [[]]
reshapeList x lst = h : reshapeList x t
  where (h, t) = splitAt x lst

randomNeuron :: (RandomGen g) => RandomTransform -> g -> Int -> Vector Double
randomNeuron transform g inputDim = inputDim |> (randomList transform g)

makeVectors :: (RandomGen g) => RandomTransform -> g -> Int -> Int -> [Vector Double]
makeVectors transform g inputDim 0 = []
makeVectors transform g inputDim num = (randomNeuron transform g' inputDim) :
                                       makeVectors transform g'' inputDim (num - 1)
  where (g', g'') = split g

distance :: Vector Double -> Vector Double -> Double
distance a b = sqrt $ sum $ map (^2) $ zipWith (-) (toList a) (toList b)

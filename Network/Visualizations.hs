{-# LANGUAGE FlexibleContexts #-}

module Network.Visualizations
( weightHistogram
, weightList
, biasList
) where

import Numeric.LinearAlgebra
import Network.Neuron
import Network.Layer
import Network.Network
import Network.Trainer
import Data.Foldable (foldMap, )

import Network.Histogram
import GHC.Float

weightList :: (Floating (Vector a), Container Vector a)
  => Network a -> [a]
weightList n = concat $ map (toList . flatten . weightMatrix) (layers n)

biasList :: (Floating (Vector a), Container Vector a)
=> Network a -> [a]
biasList n = concat $ map (toList . biasVector) (layers n)

weightHistogram :: (Floating (Vector a), Container Vector a)
  => Network a -> IO ()
weightHistogram n = do
  let hist = histogram binSturges (weightList n)
  plot "simple.png" hist
  return ()

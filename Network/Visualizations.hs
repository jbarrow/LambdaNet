{-# LANGUAGE FlexibleContexts #-}

module Network.Visualizations
( networkHistogram
, weightList
, biasList
) where

import Numeric.LinearAlgebra
import Network.Neuron
import Network.Layer
import Network.Network
import Network.Trainer
import Data.Foldable (foldMap, )

import Graphics.Histogram
import GHC.Float

weightList :: FeedForwardNetwork -> [Double]
weightList n = concat $ map (toList . flatten . weightMatrix) (layers n)

biasList :: FeedForwardNetwork -> [Double]
biasList n = concat $ map (toList . biasVector) (layers n)

networkHistogram :: FilePath -> (FeedForwardNetwork -> [Double]) -> FeedForwardNetwork -> IO ()
networkHistogram filename listFunction n = do
  let hist = histogram binSturges (listFunction n)
  plot filename hist
  return ()

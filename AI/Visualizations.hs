{-# LANGUAGE FlexibleContexts #-}

module AI.Visualizations
( networkHistogram
, weightList
, biasList
) where

import           AI.Layer
import           AI.Network
import           AI.Network.FeedForwardNetwork
import           AI.Neuron
import           Numeric.LinearAlgebra

import           Data.Foldable                      (foldMap)
import           GHC.Float
import           Graphics.Histogram
import           AI.Trainer

weightList :: FeedForwardNetwork -> [Double]
weightList = toList . flatten . weightMatrix <=< layers

biasList :: FeedForwardNetwork -> [Double]
biasList = toList . biasVector <=< layers

networkHistogram :: FilePath -> (FeedForwardNetwork -> [Double]) -> FeedForwardNetwork -> IO ()
networkHistogram filename listFunction n = do
  let hist = histogram binSturges (listFunction n)
  plot filename hist
  return ()

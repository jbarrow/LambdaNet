{-# LANGUAGE FlexibleContexts #-}

import           AI.Layer
import           AI.Neuron

import           AI.Network
import           AI.Network.FeedForwardNetwork

import           AI.Trainer
import           AI.Trainer.BackpropTrainer

--import Network.Visualizations
import           Numeric.LinearAlgebra
import           System.IO
import           System.Random

main :: IO ()
main = do

  -- Convolutional Settings
  let field = 3
  let stride = 0
  let padding = 0
  let filters = 2
  let dimensions = 1
  let widthIn = 4
  let heightIn = 4
  let widthOut = 2
  let heightOut = 2
  let connectivity = connectLocally field stride padding filters dimensions widthIn heightIn widthOut heightOut
  let randomization = randomizeLocally field stride padding filters dimensions widthIn heightIn widthOut heightOut

  g <- newStdGen
  let l = LayerDefinition sigmoidNeuron (16 * dimensions) connectFully randomizeFully
  let l' = LayerDefinition sigmoidNeuron (4 * filters) connectivity randomization

  let n = createNetwork uniforms g [l, l'] :: FeedForwardNetwork

  print $ predict (fromList [0..15]) n

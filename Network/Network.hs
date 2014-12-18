{-# LANGUAGE FlexibleContexts #-}

module Network.Network
( Network(..)
, Trainer(..)
, TrainingData

, createNetwork
, predict

) where

import Network.Neuron
import Network.Layer
import System.Random
import Numeric.LinearAlgebra

-- | Networks are constructed front to back. Start by adding an input layer,
--   then each hidden layer, and finally an output layer.
data Network a = Network { layers :: [Layer a] }

-- | A tuple of (input, expected output)
type TrainingData a = (Vector a, Vector a)

-- | Trainer is a typeclass for all trainer types - a trainer will take in
--   an instance of itself, a network, a list of training data, and return a
--   new network trained on the data.
class Trainer a where
  fit :: (Floating b) => a -> Network b -> [TrainingData b] -> Network b

-- | The createNetwork function takes in a random transform used for weight
--   initialization, a source of entropy, and a list of layer definitions,
--   and returns a network with the weights initialized per the random
--   transform.
createNetwork :: (RandomGen g, Random a, Floating a, Num (Vector a), Container Vector a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
createNetwork t g (layerDef : layerDef' : otherLayerDefs) =
  Network (layer : layers restOfNetwork)
  where layer = createLayer t g layerDef layerDef'
        restOfNetwork = createNetwork t g (layerDef' : otherLayerDefs)

-- | Predict folds over each layer of the network using the input vector as the
--   first value of the accumulator. It operates on whatever network you pass
--   in.
predict :: (Floating a, Num (Vector a), Container Vector a, Product a) => Vector a -> Network a -> Vector a
predict input network = foldl apply input (layers network)

-- | A non-public function used in the fold in predict that applies the
--   activation function and pushes the input through a layer of the
--   network.
apply :: (Floating a, Num (Vector a), Container Vector a, Product a) => Vector a -> Layer a -> Vector a
apply vector layer = mapVector sigma (weights <> vector + bias)
  where sigma = activation (neuron layer)
        weights = weightMatrix layer
        bias = (biasVector layer)

{-# LANGUAGE FlexibleContexts #-}

module Network.Network
( Network(..)
, TrainingData

, createNetwork
, predict
, apply
, saveNetwork
) where

import Network.Neuron
import Network.Layer
import System.Random
import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as B
import System.IO
import Data.Binary (encode, decode, Binary(..))

-- | Networks are constructed front to back. Start by adding an input layer,
--   then each hidden layer, and finally an output layer.
data Network a = Network { layers :: [Layer a] }

-- | A tuple of (input, expected output)
type TrainingData a = (Vector a, Vector a)

-- | The createNetwork function takes in a random transform used for weight
--   initialization, a source of entropy, and a list of layer definitions,
--   and returns a network with the weights initialized per the random transform.
createNetwork :: (RandomGen g, Random a, Floating a, Floating (Vector a), Container Vector a)
  => RandomTransform a -> g -> [LayerDefinition a] -> Network a
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
createNetwork t g (layerDef : layerDef' : otherLayerDefs) =
  Network (layer : layers restOfNetwork)
  where layer = createLayer t g layerDef layerDef'
        restOfNetwork = createNetwork t g (layerDef' : otherLayerDefs)

-- | Predict folds over each layer of the network using the input vector as the
--   first value of the accumulator. It operates on whatever network you pass in.
predict :: (Floating (Vector a), Container Vector a, Product a)
  => Vector a -> Network a -> Vector a
predict input network = foldl apply input (layers network)

-- | A function used in the fold in predict that applies the activation
--   function and pushes the input through a layer of the network.
apply :: (Floating (Vector a), Container Vector a, Product a)
  => Vector a -> Layer a -> Vector a
apply vector layer = mapVector sigma (weights <> vector + bias)
  where sigma = activation (neuron layer)
        weights = weightMatrix layer
        bias = biasVector layer

saveNetwork :: (Binary (ShowableLayer a), Floating a, Floating (Vector a), Container Vector a)
  => FilePath -> Network a -> IO ()
saveNetwork file n = B.writeFile file (encode $ map layerToShowable (layers n))

--laodNetwork :: (RandomGen g, Random a, Floating a, Floating (Vector a), Container Vector a)
--  => FilePath -> [LayerDefinition a] -> Network a

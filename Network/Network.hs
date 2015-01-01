{-# LANGUAGE FlexibleContexts,
             UndecidableInstances #-}

module Network.Network
( Network(..)
, TrainingData

, createNetwork
, loadNetwork
, emptyNetwork
, isEmptyNetwork
, addNetworks
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
import Data.Monoid (Monoid(..))
import Data.Binary (encode, decode, Binary(..))

-- | Networks are constructed front to back. Start by adding an input layer,
--   then each hidden layer, and finally an output layer.
data Network a = Network { layers :: [Layer a] }

-- | We gain the ability to combine two networks of the same proportions
--   by abstracting a network as a monoid. This is useful in backpropagation
--   for batch training
instance (Product a, Container Vector a, Floating (Vector a)) => Monoid (Network a) where
  mempty = emptyNetwork
  mappend = addNetworks

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

-- | Our Unit, an empty network with no layers
emptyNetwork :: (Product a)
  => Network a
emptyNetwork = Network []

-- | A boolean to check if the network is the unit network or not
isEmptyNetwork :: (Product a)
  => Network a -> Bool
isEmptyNetwork n = length (layers n) == 0

-- | A function to combine two networks
addNetworks :: (Floating (Vector a), Container Vector a, Product a)
  => Network a -> Network a -> Network a
addNetworks n1 n2 = if isEmptyNetwork n1 then n2 else
  if isEmptyNetwork n2 then n1 else
    Network $ zipWith combineLayers (layers n1) (layers n2)
  where combineLayers l1 l2 =
          Layer ((weightMatrix l1) + (weightMatrix l2))
          ((biasVector l1) + (biasVector l2)) (neuron l1)

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

-- | Given a filename and a network, we want to save the weights and biases
--   of the network to the file for later use.
saveNetwork :: (Binary (ShowableLayer a), Floating a, Floating (Vector a), Container Vector a)
  => FilePath -> Network a -> IO ()
saveNetwork file n = B.writeFile file (encode $ map layerToShowable (layers n))

-- | Given a filename, and a list of layer definitions, we want to reexpand
--   the data back into a network.
loadNetwork :: (Binary (ShowableLayer a), Floating a, Floating (Vector a), Container Vector a)
  => FilePath -> [LayerDefinition a] -> IO (Network a)
loadNetwork file defs = B.readFile file >>= \sls ->
  return $ Network (map showableToLayer (zip (decode sls) defs))

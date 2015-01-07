{-# LANGUAGE FlexibleContexts,
             UndecidableInstances,
             InstanceSigs #-}

module Network.Network.FeedForwardNetwork
( FeedForwardNetwork(..)

, emptyFeedForwardNetwork
, isEmptyFeedForwardNetwork
, addFeedForwardNetworks

, loadFeedForwardNetwork
, saveFeedForwardNetwork

, apply
) where

import Network.Neuron
import Network.Layer
import Network.Network

import System.Random
import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as B
import System.IO
import Data.Monoid (Monoid(..))
import Data.Binary (encode, decode, Binary(..))

-- | Networks are constructed front to back. Start by adding an input layer,
--   then each hidden layer, and finally an output layer.
data FeedForwardNetwork = FeedForwardNetwork { layers :: [Layer] } deriving Show

-- | We gain the ability to combine two networks of the same proportions
--   by abstracting a network as a monoid. This is useful in backpropagation
--   for batch training
instance Monoid (FeedForwardNetwork) where
  mempty = emptyFeedForwardNetwork
  mappend = addFeedForwardNetworks

-- | A tuple of (input, expected output)
type TrainingData = (Vector Double, Vector Double)

-- | Our Unit, an empty network with no layers
emptyFeedForwardNetwork :: FeedForwardNetwork
emptyFeedForwardNetwork = FeedForwardNetwork []

-- | A boolean to check if the network is the unit network or not
isEmptyFeedForwardNetwork :: FeedForwardNetwork -> Bool
isEmptyFeedForwardNetwork n = length (layers n) == 0

-- | A function to combine two networks
addFeedForwardNetworks :: FeedForwardNetwork -> FeedForwardNetwork -> FeedForwardNetwork
addFeedForwardNetworks n1 n2 = if isEmptyFeedForwardNetwork n1 then n2 else
  if isEmptyFeedForwardNetwork n2 then n1 else
    FeedForwardNetwork $ zipWith combineLayers (layers n1) (layers n2)
  where combineLayers l1 l2 =
          Layer ((weightMatrix l1) + (weightMatrix l2))
          ((biasVector l1) + (biasVector l2)) (neuron l1)

instance Network (FeedForwardNetwork) where
  -- | Predict folds over each layer of the network using the input vector as the
  --   first value of the accumulator. It operates on whatever network you pass in.
  predict :: Vector Double -> FeedForwardNetwork -> Vector Double
  predict input network = foldl apply input (layers network)

  -- | The createNetwork function takes in a random transform used for weight
  --   initialization, a source of entropy, and a list of layer definitions,
  --   and returns a network with the weights initialized per the random transform.
  createNetwork :: RandomGen g => RandomTransform -> g -> [LayerDefinition] -> FeedForwardNetwork
  createNetwork t g [] = emptyFeedForwardNetwork
  createNetwork t g (layerDef : []) = emptyFeedForwardNetwork
  createNetwork t g (layerDef : layerDef' : otherLayerDefs) =
    FeedForwardNetwork (layer : layers restOfNetwork)
    where layer = createLayer t g' layerDef layerDef'
          restOfNetwork = createNetwork t g'' (layerDef' : otherLayerDefs)
          (g', g'') = split g

-- | A function used in the fold in predict that applies the activation
--   function and pushes the input through a layer of the network.
apply :: Vector Double -> Layer -> Vector Double
apply vector layer = mapVector sigma (weights <> vector + bias)
  where sigma = activation (neuron layer)
        weights = weightMatrix layer
        bias = biasVector layer

-- | Given a filename and a network, we want to save the weights and biases
--   of the network to the file for later use.
saveFeedForwardNetwork :: Binary (Layer) => FilePath -> FeedForwardNetwork -> IO ()
saveFeedForwardNetwork file n = B.writeFile file (encode (layers n))

-- | Given a filename, and a list of layer definitions, we want to reexpand
--   the data back into a network.
loadFeedForwardNetwork :: Binary (Layer) => FilePath -> [LayerDefinition] -> IO (FeedForwardNetwork)
loadFeedForwardNetwork file defs = B.readFile file >>= \sls ->
  return $ FeedForwardNetwork (map showableToLayer (zip (decode sls) defs))

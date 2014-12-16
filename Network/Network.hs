module Network.Network
( Network(..)
, Trainer(..)

, CostFunction
, CostFunction'
, TrainingData

, createNetwork
) where

import Network.Neuron
import Network.Layer
import Linear
import System.Random
import qualified Data.Map as Map

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.

data Network a = Network { layers :: [Layer a] }

-- createNetwork
--   creates a neural network
-- parameters
--   t = transform function (e.g. uniforms, normals)
--   g = random generator (e.g. mkStdGen 4)
--   layers = a list of LayerDefinitions
-- returns
--   a network with layers defined by the list of layer definitions
createNetwork :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
-- Base Cases
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
-- Return a layer ++ the rest of the network
createNetwork t g (layerDef : (layerDef' : otherLayerDefs)) =
  Network (aLayer : layers restOfNetwork)
  where aLayer = createLayer t g layerDef layerDef'
        restOfNetwork = createNetwork t g (layerDef' : otherLayerDefs)

-- Using a scikit-learn-esque naming scheme - predict to classify data
-- and fit to train the network.
predict :: (Floating a) => Vector a -> Network a -> Vector a
predict input network = input


fit :: (Floating a, Trainer t) => t -> Int -> [TrainingData a] -> Network a -> Network a
fit t batch (h:ts) network = train t network h

-- Training a network

type CostFunction a = a -> a
type CostFunction' a = a -> a

type TrainingData a = Map.Map (Vector a) (Vector a)

data BackpropTrainer a = BackpropTrainer { eta :: a
                                         , cost :: CostFunction a
                                         , cost' :: CostFunction' a
                                         }

class Trainer a where
  train :: (Floating b) => a -> Network b -> TrainingData b -> Network b

instance (Floating a) => Trainer (BackpropTrainer a) where
  train trainer network trainData = network

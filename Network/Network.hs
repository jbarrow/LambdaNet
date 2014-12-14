module Network.Network
( Network(..)

, createLayer
, createNetwork
) where

import Network.Neuron
import Network.Layer
import Network.Trainer
import Linear
import System.Random

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.

data Network a = Network { layers :: [Layer a] }

-- t is transform
-- g is a random number generator
createNetwork :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
-- Base Cases
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
-- Return a layer ++ the rest of the network
createNetwork t g (layerDef : (layerDef' : otherLayerDefs)) =
  Network (aLayer : layers restOfNetwork)
  where aLayer = createLayer t g layerDef layerDef'
        restOfNetwork = createNetwork t g (layerDef' : otherLayerDefs)

createLayer :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> LayerDefinition a -> LayerDefinition a -> Layer a
createLayer t g layerDef layerDef' =
  Layer (hadamard randomMatrix (connectivity i j))
        (hadamard randomMatrix' (connectivity i j))
        (neuronDef layerDef)
  where randomMatrix = reshape j (take (i*j) (randomList t g))
        randomMatrix' = reshape j (take (i*j) (randomList t g))
        i = neuronCount layerDef
        j = neuronCount layerDef'
        connectivity = connect layerDef

addLayerDefinition :: (Floating a) => LayerDefinition a -> [LayerDefinition a] -> [LayerDefinition a]
addLayerDefinition layer layers = (layers ++ [layer])

predict :: (Floating a) => Vector a -> Network a -> Vector a
predict input network = input

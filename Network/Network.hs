module Network.Network
( Network(..)

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
createNetwork :: (RandomGen g, Floating a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
createNetwork t g (layerDef : (layerDef' : otherLayerDefs)) =
  Network (createLayer t g layerDef layerDef' : layers (createNetwork t g (layerDef' : otherLayerDefs)))

createLayer :: (RandomGen g, Floating a) => RandomTransform a -> g -> LayerDefinition a -> LayerDefinition a -> Layer a
createLayer t g layerDef layerDef' =
  Layer ((connect layerDef) (neuronCount layerDef) (neuronCount layerDef'))
        ((connect layerDef) (neuronCount layerDef) (neuronCount layerDef'))
        (neuronDef layerDef)

-- randomizeValues :: (RandomGen g, Floating a) => RandomTransform a -> g -> Matrix a -> Matrix a
-- randomizeValues t g matrix =
--   take

addLayerDefinition :: (Floating a) => LayerDefinition a -> [LayerDefinition a] -> [LayerDefinition a]
addLayerDefinition layer layers = (layers ++ [layer])

predict :: (Floating a) => Vector a -> Network a -> Vector a
predict input network = input

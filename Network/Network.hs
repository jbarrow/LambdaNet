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
--data Network a = Network [LayerDefinition a]

type Network a = [(Matrix a, Matrix a)]

-- t is transform
-- g is a random number generator
createNetwork :: (RandomGen g, Floating a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
createNetwork t g [] = []
createNetwork t g (layerDef : []) = []
createNetwork t g (layerDef : (layerDef' : otherLayerDefs)) =
  connectLayers layerDef layerDef' : createNetwork t g (layerDef' : otherLayerDefs)

-- returns
--   a tuple of the weight matrix and the bias matrix
connectLayers :: (Floating a) => LayerDefinition a -> LayerDefinition a -> (Matrix a, Matrix a)
connectLayers layerDef layerDef' =
  ((connect layerDef) (neuronCount layerDef) ( neuronCount layerDef'),
   (connect layerDef) (neuronCount layerDef) (neuronCount layerDef'))

addLayerDefinition :: (Floating a) => LayerDefinition a -> [LayerDefinition a] -> [LayerDefinition a]
addLayerDefinition layer layers = (layers ++ [layer])

predict :: (Floating a) => Vector a -> Network a -> Vector a
predict input network = input

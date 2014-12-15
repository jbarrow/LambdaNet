module Network.Network
( Network(..)

, createNetwork
) where

import Network.Neuron
import Network.Layer
import Linear
import System.Random

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

-- addLayerDefinition :: (Floating a) => LayerDefinition a -> [LayerDefinition a] -> [LayerDefinition a]
-- addLayerDefinition layer layers = (layers ++ [layer])

predict :: (Floating a) => Vector a -> Network a -> Vector a
predict input network = input

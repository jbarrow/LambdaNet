module Network.Network
( Network(..)

) where

import Network.Neuron
import Network.Layer
import Network.Trainer
import Linear

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.
--data Network a = Network [LayerDefinition a]

type Network a = [(Matrix a, Matrix a)]

createNetwork :: (Floating a) => [LayerDefinition a] -> Network a
createNetwork [] = []
createNetwork (layerDef : []) = []
createNetwork (layerDef : (layerDef' : otherLayerDefs)) =
  ( (connect layerDef) (neuronCount layerDef) (neuronCount layerDef'),
    (connect layerDef) (neuronCount layerDef) (neuronCount layerDef'))
  : createNetwork (layerDef' : otherLayerDefs)

addLayerDefinition :: (Floating a) => LayerDefinition a -> [LayerDefinition a] -> [LayerDefinition a]
addLayerDefinition layer layers = (layers ++ [layer])

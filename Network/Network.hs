module Network.Network
( Network(..)

) where

import Network.Neuron
import Network.Layer
import Network.Trainer

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.
data Network a = Network [Layer a]

addLayerDefinition :: (Floating a) => LayerDefinition a -> [LayerDefinition a] -> [LayerDefinition a]
addLayerDefinition layer layers = (layers ++ [layer])

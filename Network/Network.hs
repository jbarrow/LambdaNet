module Network.Network
( Network(..)


) where

import Network.Neuron

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.
data Network a = Network

addLayerDefinition :: (Floating a) => Network a -> Layer a -> Network a
addLayerDefinition (Network layers) layer = Network (layers ++ [layer])

buildNetwork :: [LayerDefinitions] -> Network

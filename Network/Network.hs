module Network.Network
( Network(..)
, Layer(..)

, createNetwork
, addLayer
) where

import Network.Neuron

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.
data Network a = Network [Layer a]

data Layer a = Layer (Neuron a) Int

createNetwork :: (Floating a) => Network a
createNetwork = Network []

addLayer :: (Floating a) => Network a -> Layer a -> Network a
addLayer (Network layers) layer = Network (layers ++ [layer])

module Network.Network
( Network(..)
, Layer(..)
) where

import qualified Network.Neuron as Neuron

data Network = Network [Layer]

data Layer = Layer { layer_type :: Neuron.Neuron
                   , count :: Int
                   }

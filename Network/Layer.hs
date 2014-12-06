module Network.Layer
( LayerDefinition(..)

, Connectivity
) where

import Network.Neuron

data LayerDefinition a = LayerDefinition (Neuron a) Int (Connectivity a)

type Connectivity a = Int -> Int -> [[a]]

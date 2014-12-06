module Network.Layer
( Layer(..)
, LayerDefinition(..)

, Connectivity
) where

data LayerDefinition a = LayerDefinition (Neuron a) Int Connectivity

type Connectivity a = Int -> Int -> [[a]]

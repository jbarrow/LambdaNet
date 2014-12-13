module Network.Layer
( LayerDefinition(..)
, Layer

, Connectivity

, applyLayer
) where

import Network.Neuron
import Linear

data LayerDefinition a = LayerDefinition (Neuron a) Int (Connectivity a)

type Layer a = (Matrix a)

type Connectivity a = Int -> Int -> Matrix a

applyLayer :: (Floating a) => Layer a -> Vector a -> Vector a
applyLayer layer d = d

module Network.Network.SOM
( SOM(..)

) where

import Network.Neuron
import Network.Layer
import Network.Network

import Numeric.LinearAlgebra

data SOM = SOM { map :: Matrix (Vector Double) }

instance Network (SOM) where
  predict inputs network = inputs

  createNetwork transformation g defs = SOM $ fromLists [[fromList [1.0] :: Vector Double]]

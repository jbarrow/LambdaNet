{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecideableInstances #-}

module Network.Network.SOM
( SOM(..)

) where

import           Network.Layer
import           Network.Network
import           Network.Neuron

import           Numeric.LinearAlgebra

data SOM = SOM { map :: Matrix (Vector Double) }

data MapDefinition = MapDefinition { x   :: Int,
                                     y   :: Int,
                                     dim :: Int }

instance Network (SOM) where
  type Parameters = MapDefinition

  predict :: Vector Double -> SOM -> Vector Double
  predict inputs network = inputs

  createNetwork :: (RandomGen g) => RandomTransform -> g -> MapDefinition -> SOM
  createNetwork transformation g defs = SOM $ fromLists [[fromList [1.0] :: Vector Double]]


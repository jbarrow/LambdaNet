{-# LANGUAGE FlexibleContexts,
             UndecidableInstances,
             InstanceSigs #-}

module Network.Network
( Network(..) ) where

import Network.Layer
import System.Random
import Numeric.LinearAlgebra

class Network a where
  predict :: Vector Double -> a -> Vector Double
  createNetwork :: (RandomGen g) => RandomTransform -> g -> [LayerDefinition] -> a

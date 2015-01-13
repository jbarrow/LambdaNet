{-# LANGUAGE FlexibleContexts,
             UndecidableInstances,
             TypeFamilies #-}

module Network.Network
( Network(..) ) where

import Network.Layer
import System.Random
import Numeric.LinearAlgebra

-- | A network is
class Network a where
  type Parameters :: *

  predict :: Vector Double -> a -> Vector Double
  createNetwork :: (RandomGen g) => RandomTransform -> g -> Parameters -> a

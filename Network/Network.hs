{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Network
( Network(..) ) where

import           Network.Layer
import           Numeric.LinearAlgebra
import           System.Random

-- | A network is
class Network a where
  type Parameters :: *

  predict :: Vector Double -> a -> Vector Double
  createNetwork :: (RandomGen g) => RandomTransform -> g -> Parameters -> a

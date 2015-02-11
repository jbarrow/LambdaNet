{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module AI.Network
( Network(..) ) where

import           AI.Layer
import           Numeric.LinearAlgebra
import           System.Random

-- | A network is
class Network a where
  type Parameters :: *

  predict :: Vector Double -> a -> Vector Double
  createNetwork :: (RandomGen g) => RandomTransform -> g -> Parameters -> a

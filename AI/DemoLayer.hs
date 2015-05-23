{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module AI.DemoLayer
( LayerDefinition(..)
, Layer(..)

, Weights
, Biases
, LayerWidth
, Connectivity

--, createLayer
) where

import AI.DemoNeuron

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import System.Random

data LayerDefinition a = LayerDefinition { neuronType   :: a
                                         , width        :: LayerWidth
                                         , connectivity :: Connectivity
                                         }

data Layer a = Layer { neuron :: a
                     , weights :: Weights
                     , biases :: Biases
                     } deriving (Show)

type Weights = Matrix Double
type Biases = Vector Double
type LayerWidth = Int
type Connectivity = LayerWidth -> LayerWidth -> Weights

instance (Neuron a) => Binary (Layer a) where
  put Layer{..} = do put weights; put biases
  get = do weights <- get; biases <- get; return Layer{..}

-- createLayer :: (Neuron a, RandomGen g)
--               => RandomTransform -> g -> LayerDefinition a
--               -> LayerDefinition a -> Layer
-- createLayer t g li lj = Layer (neuron lj) 

module Network.Trainer
( Trainer(..)

, CostFunction
, CostFunction'
) where

import Linear
import Network.Network
import qualified Data.Map as Map

type CostFunction a = a -> a
type CostFunction' a = a -> a

type TrainingData a = Map.Map (Vector a) (Vector a)

class Trainer a where
  train :: Network a -> TrainingData a -> Network a

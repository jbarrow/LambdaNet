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

data BackpropTrainer a = BackpropTrainer { eta :: a
                                         , cost :: CostFunction a
                                         , cost' :: CostFunction' a
                                         }

class Trainer a where
  train :: (Floating b) => a -> Network b -> TrainingData b -> Network b

instance (Floating a) => Trainer (BackpropTrainer a) where
  train trainer network goals = network

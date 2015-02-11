{-# InstanceSigs #-}

module AI.Trainer.SOMTrainer
( SOMTrainer(..)
, fit
, evaluate
) where

import AI.Trainer
import AI.Network.SOM

data SOMTrainer = SOMTrainer { eta :: Double }

instance Trainer SOMTrainer SOM where
  fit selection trainer network data = n

  evaluate t n e = 0.0

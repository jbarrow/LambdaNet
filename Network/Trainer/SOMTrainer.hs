module Network.Trainer.SOMTrainer
( SOMTrainer(..)
, fit
, evaluate
) where

import Network.Trainer
import Network.Network.SOM

data SOMTrainer = SOMTrainer { eta :: Double }

instance Trainer SOMTrainer SOM where
  fit selection trainer network data = n

  evaluate t n e = 0.0

module Network.Trainer
( Trainer(..)

, TrainingData
, TestingData
) where

import Linear

data Trainer = Trainer

-- We define a training data type that is a tuple vectors, where
-- fst TrainingData is the input and snd TrainigData is the expected
-- output.
type TrainingData a = (Vector a, Vector a)
-- Similarly, we define testing data as a vector of data types
type TestingData a = Vector a

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Trainer
( Trainer(..)
, CostFunction
, CostFunction'
, TrainingData
, Selection
, StopCondition

, quadraticCost
, quadraticCost'
, minibatch
, online

, trainNTimes
, trainUntilErrorLessThan
, trainUntil
) where

import           Network.Layer
import           Network.Network
import           Network.Neuron

import           Data.List.Split       (chunksOf)
import           Numeric.LinearAlgebra
import           System.Random
import           System.Random.Shuffle (shuffle')

-- | Trainer is a typeclass for all trainer types - a trainer will take in
--   an instance of itself, a network, a list of training data, and return a
--   new network trained on the data.
class (Network n) => Trainer a n where
  fit :: Selection -> a -> n -> [TrainingData] -> n
  evaluate :: a -> n -> TrainingData -> Double

-- | A CostFunction is used for evaluating a network's performance on a given
--   input
type CostFunction = Vector Double -> Vector Double -> Double

-- | A CostFunction' (derivative) is used in backPropagation
type CostFunction' = Vector Double -> Vector Double -> Vector Double

-- | A tuple of (input, expected output)
type TrainingData = (Vector Double, Vector Double)

-- | A selection function for performing gradient descent
type Selection = [TrainingData] -> [[TrainingData]]

-- | A predicate (given a network, trainer, a list of training
--   data, and the number of [fit]s performed) that
--   tells the trainer to stop training
type StopCondition t n = n -> t -> [TrainingData] -> Int -> Bool

-- | The quadratic cost function (1/2) * sum (y - a) ^ 2
quadraticCost :: Vector Double -> Vector Double -> Double
quadraticCost y a = sumElements $ 0.5 * (a - y) ** 2

-- | The derivative of the quadratic cost function sum (y - a)
quadraticCost' :: Vector Double -> Vector Double -> Vector Double
quadraticCost' y a = a - y

-- | The minibatch function becomes a Selection when partially applied
--   with the minibatch size
minibatch :: Int -> [TrainingData] -> [[TrainingData]]
minibatch size = chunksOf size

-- | If we want to train the network online
online :: [TrainingData] -> [[TrainingData]]
online = minibatch 1

-- | This function returns true if the error of the network is less than
--   a given error value, given a network, a trainer, a list of
--   training data, and a counter (should start with 0)
--   Note: Is there a way to have a counter with a recursive function
--         without providing 0?
networkErrorLessThan :: (Trainer t n) => Double -> n -> t -> [TrainingData] -> Int -> Bool
networkErrorLessThan err network trainer dat _ = meanError < err
  where meanError = (sum errors) / fromIntegral (length errors)
        errors = map (evaluate trainer network) dat

-- | Given a network, a trainer, a list of training data,
--   and N, this function trains the network with the list of
--   training data N times
trainNTimes :: (Trainer t n, RandomGen g) => g -> n -> t -> Selection -> [TrainingData] -> Int -> n
trainNTimes g network trainer s dat n =
  trainUntil g network trainer s dat completion 0
  where completion _ _ _ n' = (n == n')

-- | Given a network, a trainer, a list of training data,
--   and an error value, this function trains the network with the list of
--   training data until the error of the network (calculated
--   by averaging the errors of each training data) is less than
--   the given error value
trainUntilErrorLessThan :: (Trainer t n, RandomGen g) => g -> n -> t -> Selection -> [TrainingData] -> Double -> n
trainUntilErrorLessThan g network trainer s dat err =
  trainUntil g network trainer s dat (networkErrorLessThan err) 0

-- | This function trains a network until a given StopCondition
--   is satisfied.
trainUntil :: (Trainer t n, RandomGen g) => g -> n -> t -> Selection -> [TrainingData] -> StopCondition t n -> Int -> n
trainUntil g network trainer s dat completion n =
  if completion network trainer dat n
    then network
    else trainUntil g' network' trainer s (shuffle' dat (length dat) g'') completion (n+1)
    where network' = fit s trainer network dat
          (g', g'') = split g

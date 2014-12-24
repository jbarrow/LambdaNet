{-# LANGUAGE FlexibleContexts #-}

module Network.Trainer
( BackpropTrainer(..)
, CostFunction
, CostFunction'

, quadraticCost
, quadraticCost'
, backprop
, inputs
, outputs
, deltas
, fit
) where

import Network.Network
import Network.Neuron
import Network.Layer
import System.Random
import Numeric.LinearAlgebra

-- | Trainer is a typeclass for all trainer types - a trainer will take in
--   an instance of itself, a network, a list of training data, and return a
--   new network trained on the data.
--class Trainer a where
--  fit :: (Floating b) => a -> Network b -> [TrainingData b] -> Network b

-- | A BackpropTrainer performs simple backpropagation on a neural network.
--   It can be used as the basis for more complex trainers.
data BackpropTrainer a = BackpropTrainer { eta :: a
                                         , cost :: CostFunction a
                                         , cost' :: CostFunction' a
                                         }

-- | A CostFunction is used for evaluating a network's performance on a given
--   input
type CostFunction a = Vector a -> Vector a -> a

-- | A CostFunction' (derivative) is used in backPropagation
type CostFunction' a = Vector a -> Vector a -> Vector a

-- | The quadratic cost function (1/2) * sum (y - a) ^ 2
quadraticCost :: (Floating (Vector a), Container Vector a)
  => Vector a -> Vector a -> a
quadraticCost y a = sumElements $ 0.5 * (a - y) ** 2

-- | The derivative of the quadratic cost function sum (y - a)
quadraticCost' :: (Floating (Vector a))
  => Vector a -> Vector a -> Vector a
quadraticCost' y a = a - y

-- | Declare the BackpropTrainer to be an instance of Trainer.
--instance (Floating a) => Trainer (BackpropTrainer a) where
fit :: (Floating (Vector a), Container Vector a, Product a)
  => BackpropTrainer a -> Network a -> [TrainingData a] -> Network a
fit t n [] = n
fit t n trainData = fit t (backprop t n (head trainData)) (tail trainData)

-- | Perform backpropagation on a single training data instance.
backprop :: (Floating (Vector a), Container Vector a, Product a)
  => BackpropTrainer a -> Network a -> TrainingData a -> Network a
backprop t n trainData = updateNetwork t n trainData (deltas t n trainData) (outputs (fst trainData) n)

updateNetwork :: (Floating (Vector a), Container Vector a, Product a)
  => BackpropTrainer a -> Network a -> TrainingData a -> [Vector a] -> [Vector a] -> Network a
updateNetwork t n example [] os = n
updateNetwork t n example (delta:ds) (output:os) =
  Network ((updateLayer t (head $ layers n) delta output) :
    (layers (updateNetwork t rest example ds os)))
  where rest = Network ([last $ layers n])

updateLayer :: (Floating (Vector a), Container Vector a, Product a)
  => BackpropTrainer a -> Layer a -> Vector a -> Vector a -> Layer a
updateLayer t l delta output = Layer newWeight newBias n
  where n = neuron l
        newWeight = (weightMatrix l) - (eta t) `scale` ((reshape 1 output) <> (reshape (dim delta) delta))
        newBias = (biasVector l) - (eta t) `scale` delta

-- | The outputs function scans over each layer of the network and stores the
--   activated results
outputs :: (Floating (Vector a), Container Vector a, Product a)
  => Vector a -> Network a -> [Vector a]
outputs input network = scanl apply input (layers network)

-- | The inputs function performs a similar task to outputs, but returns a list
--   of vectors of unactivated inputs
inputs :: (Floating (Vector a), Container Vector a, Product a)
  => Vector a -> Network a -> [Vector a]
inputs input network = if null (layers network) then []
  else unactivated : inputs activated (Network (tail $ layers network))
    where unactivated = weightMatrix layer <> input + biasVector layer
          layer = head (layers network)
          activated = mapVector (activation (neuron layer)) unactivated

-- | The deltas function returns a list of layer deltas.
deltas :: (Floating (Vector a), Container Vector a, Product a)
  => BackpropTrainer a -> Network a -> TrainingData a -> [Vector a]
deltas t n example = hiddenDeltas
  (Network (reverse (layers n))) outputDelta (tail $ reverse is)
    ++ [outputDelta]
  where outputDelta = costd (snd example) output *
          mapVector activationd lastInput
        costd = cost' t
        activationd = activation' (neuron (last (layers n)))
        output = last os
        lastInput = last is
        is = inputs (fst example) n
        os = outputs (fst example) n

-- | Compute the hidden layer deltas
hiddenDeltas :: (Floating (Vector a), Container Vector a, Product a)
  => Network a -> Vector a -> [Vector a] -> [Vector a]
hiddenDeltas n prevDelta is = if length (layers n) <= 1 then []
  else delta : hiddenDeltas rest delta (tail is)
  where rest = Network (tail $ layers n)
        delta = (trans w) <> prevDelta * spv
        w = weightMatrix (head $ layers n)
        spv = mapVector (activation' (neuron (head $ layers n))) (head is)

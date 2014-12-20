{-# LANGUAGE FlexibleContexts #-}

module Network.Trainer
( Trainer(..)
, BackpropTrainer(..)
, CostFunction
, CostFunction'

, quadraticCost
, quadraticCost'
, backprop
, inputs
, deltas
) where

import Network.Network
import Network.Neuron
import Network.Layer
import System.Random
import Numeric.LinearAlgebra

-- | Trainer is a typeclass for all trainer types - a trainer will take in
--   an instance of itself, a network, a list of training data, and return a
--   new network trained on the data.
class Trainer a where
  fit :: (Floating b) => a -> Network b -> [TrainingData b] -> Network b

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
quadraticCost :: (Floating a, Num (Vector a), Container Vector a)
  => Vector a -> Vector a -> a
quadraticCost y a = sumElements $ 0.5 * (y - a) ^ 2

-- | The derivative of the quadratic cost function sum (y - a)
quadraticCost' :: (Floating a, Num (Vector a), Container Vector a)
  => Vector a -> Vector a -> Vector a
quadraticCost' y a = y - a

-- | Declare the BackpropTrainer to be an instance of Trainer.
instance (Floating a) => Trainer (BackpropTrainer a) where
  fit t n [] = n
  fit t n trainData = fit t (backprop t n (head trainData)) (tail trainData)

-- | Perform backpropagation on a single training data instance.
backprop :: (Floating a, Trainer t)
  => t -> Network a -> TrainingData a -> Network a
backprop t n trainData = n--updateNetwork t n trainData (deltas t n trainData)

updateNetwork :: (Floating a, Trainer t)
  => t -> Network a -> TrainingData a -> [Vector a] -> Network a
updateNetwork t n example deltas = n

-- | The outputs function scans over each layer of the network and stores the
--   activated results
outputs :: (Floating a, Num (Vector a), Container Vector a, Product a)
  => Vector a -> Network a -> [Vector a]
outputs input network = scanl apply input (layers network)

-- | The inputs function performs a similar task to outputs, but returns a list
--   of vectors of unactivated inputs
inputs :: (Floating a, Num (Vector a), Container Vector a, Product a)
  => Vector a -> Network a -> [Vector a]
inputs input network = if null (layers network) then []
  else unactivated : inputs activated (Network (tail $ layers network))
    where unactivated = weightMatrix layer <> input + biasVector layer
          layer = head (layers network)
          activated = mapVector (activation (neuron layer)) unactivated

-- | The deltas function returns a list of layer deltas.
deltas :: (Floating a, Num (Vector a), Container Vector a, Product a)
  => BackpropTrainer a -> Network a -> TrainingData a -> [Vector a]
deltas t n example = hiddenDeltas
  (Network (reverse (layers n))) outputDelta (reverse is)
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
hiddenDeltas :: (Floating a, Num (Vector a), Container Vector a, Product a)
  => Network a -> Vector a -> [Vector a] -> [Vector a]
hiddenDeltas n prevDelta is = if length (layers n) <= 1 then []
  else delta : hiddenDeltas rest delta (tail is)
  where rest = Network (tail $ layers n)
        delta = (trans w) <> prevDelta * spv
        w = weightMatrix (head $ layers n)
        spv = mapVector (activation (neuron (head $ layers n))) (head is)

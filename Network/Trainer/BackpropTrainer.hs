{-# LANGUAGE FlexibleContexts, InstanceSigs #-}

module Network.Trainer.BackpropTrainer
( BackpropTrainer(..)

, backprop
, inputs
, outputs
, deltas
, hiddenDeltas
, calculateNablas
, fit
, evaluate
) where

import Network.Network
import Network.Neuron
import Network.Layer
import Network.Trainer

import System.Random
import System.Random.Shuffle (shuffle')
import Numeric.LinearAlgebra

-- | A BackpropTrainer performs simple backpropagation on a neural network.
--   It can be used as the basis for more complex trainers.
data BackpropTrainer = BackpropTrainer { eta :: Double
                                       , cost :: CostFunction
                                       , cost' :: CostFunction'
                                       }

-- | Declare the BackpropTrainer to be an instance of Trainer.
instance Trainer (BackpropTrainer) where
  fit :: Selection -> BackpropTrainer -> Network -> [TrainingData] -> Network
  fit s t n examples = foldl (backprop t) n $
    s (shuffle' examples (length examples) (mkStdGen 4))
  -- | Use the cost function to determine the error of a network
  evaluate :: BackpropTrainer -> Network -> TrainingData -> Double
  evaluate t n example = (cost t) (snd example) (predict (fst example) n)

-- | Perform backpropagation on a single training data instance.
backprop :: BackpropTrainer -> Network -> [TrainingData] -> Network
backprop t n es =
  updateNetwork (length es) t (foldl (calculateNablas t n) emptyNetwork es) n

-- | Given the size of the minibatch, the trainer, the nablas for each layer, given
--   as a network, and the network itself, return a network with updated wieghts.
updateNetwork :: Int -> BackpropTrainer -> Network -> Network -> Network
updateNetwork mag t nablas n = addNetworks n
  (Network $ map (scaleLayer $ -1 * (eta t) / (fromIntegral mag)) (layers nablas))

-- | Calculate the nablas for a minibatch and return them as a network (so each
--   weight and bias gets its own nabla).
calculateNablas :: BackpropTrainer -> Network -> Network -> TrainingData -> Network
calculateNablas t n nablas e = Network $ map (updateLayer t) (zip3 (layers n) ds os)
  where ds = deltas t n e
        os = outputs (fst e) n

-- | The mapped function to update the weight and biases in a single layer
updateLayer :: BackpropTrainer -> (Layer, Vector Double, Vector Double) -> Layer
updateLayer t (l, delta, output) = Layer newWeight newBias n
  where n = neuron l
        newWeight = ((reshape 1 delta) <> (reshape (dim output) output))
        newBias = delta

-- | The outputs function scans over each layer of the network and stores the
--   activated results
outputs :: Vector Double -> Network -> [Vector Double]
outputs input network = scanl apply input (layers network)

-- | The inputs function performs a similar task to outputs, but returns a list
--   of vectors of unactivated inputs
inputs :: Vector Double -> Network -> [Vector Double]
inputs input network = if null (layers network) then []
  else unactivated : inputs activated (Network (tail $ layers network))
  where unactivated = weightMatrix layer <> input + biasVector layer
        layer = head $ layers network
        activated = mapVector (activation (neuron layer)) unactivated

-- | The deltas function returns a list of layer deltas.
deltas :: BackpropTrainer -> Network -> TrainingData -> [Vector Double]
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
hiddenDeltas :: Network -> Vector Double -> [Vector Double] -> [Vector Double]
hiddenDeltas n prevDelta is = if length (layers n) <= 1 then []
  else delta : hiddenDeltas rest delta (tail is)
  where rest = Network (tail $ layers n)
        delta = (trans w) <> prevDelta * spv
        w = weightMatrix (head $ layers n)
        spv = mapVector (activation' (neuron (head $ layers n))) (head is)

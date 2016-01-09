{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AI.Trainer.BackpropTrainer
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

import           AI.Layer
import           AI.Network
import           AI.Network.FeedForwardNetwork
import           AI.Neuron
import           AI.Trainer

import           Numeric.LinearAlgebra

-- | A BackpropTrainer performs simple backpropagation on a neural network.
--   It can be used as the basis for more complex trainers.
data BackpropTrainer = BackpropTrainer { eta   :: Double
                                       , cost  :: CostFunction
                                       , cost' :: CostFunction'
                                       }

-- | Declare the BackpropTrainer to be an instance of Trainer.
instance Trainer BackpropTrainer FeedForwardNetwork where
  fit s t n examples = foldl (backprop t) n $ s examples
  -- | Use the cost function to determine the error of a network
  evaluate t n example = cost t (snd example) (predict (fst example) n)

-- | Perform backpropagation on a single training data instance.
backprop :: BackpropTrainer -> FeedForwardNetwork -> [TrainingData] -> FeedForwardNetwork
backprop t n es =
  updateNetwork (length es) t (foldl (calculateNablas t n) emptyFeedForwardNetwork es) n

-- | Given the size of the minibatch, the trainer, the nablas for each layer, given
--   as a network, and the network itself, return a network with updated wieghts.
updateNetwork :: Int -> BackpropTrainer -> FeedForwardNetwork -> FeedForwardNetwork -> FeedForwardNetwork
updateNetwork mag t nablas n = addFeedForwardNetworks n
  (FeedForwardNetwork $ map (scaleLayer $ -1 * eta t / fromIntegral mag) (layers nablas))

-- | Calculate the nablas for a minibatch and return them as a network (so each
--   weight and bias gets its own nabla).
calculateNablas :: BackpropTrainer -> FeedForwardNetwork -> FeedForwardNetwork -> TrainingData -> FeedForwardNetwork
calculateNablas t n nablas e = addFeedForwardNetworks nablas
  (FeedForwardNetwork $ map (updateLayer t) (zip3 (layers n) ds os))
  where ds = deltas t n e
        os = outputs (fst e) n

-- | The mapped function to update the weight and biases in a single layer
updateLayer :: BackpropTrainer -> (Layer, Vector Double, Vector Double) -> Layer
updateLayer t (l, delta, output) = Layer newWeight newBias n
  where n = neuron l
        newWeight = scalar $ dot delta output
        newBias = delta

-- | The outputs function scans over each layer of the network and stores the
--   activated results
outputs :: Vector Double -> FeedForwardNetwork -> [Vector Double]
outputs input network = scanl apply input (layers network)

-- | The inputs function performs a similar task to outputs, but returns a list
--   of vectors of unactivated inputs
inputs :: Vector Double -> FeedForwardNetwork -> [Vector Double]
inputs input network = if null (layers network) then []
  else unactivated : inputs activated (FeedForwardNetwork (tail $ layers network))
  where unactivated = weightMatrix layer #> input + biasVector layer
        layer = head $ layers network
        activated = cmap (activation (neuron layer)) unactivated

-- | The deltas function returns a list of layer deltas.
deltas :: BackpropTrainer -> FeedForwardNetwork -> TrainingData -> [Vector Double]
deltas t n example = reverse (hiddenDeltas
  (FeedForwardNetwork (reverse (layers n))) outputDelta (tail $ reverse is)) ++ [outputDelta]
  where outputDelta = costd (snd example) output *
          cmap activationd lastInput
        costd = cost' t
        activationd = activation' (neuron (last (layers n)))
        output = last os
        lastInput = last is
        is = inputs (fst example) n
        os = outputs (fst example) n

-- | Compute the hidden layer deltas
hiddenDeltas :: FeedForwardNetwork -> Vector Double -> [Vector Double] -> [Vector Double]
hiddenDeltas n prevDelta is = if length (layers n) <= 1 then []
  else delta : hiddenDeltas rest delta (tail is)
  where rest = FeedForwardNetwork (tail $ layers n)
        delta = tr w #> prevDelta * spv
        w = weightMatrix (head $ layers n)
        spv = cmap (activation' (neuron (head $ layers n))) (head is)

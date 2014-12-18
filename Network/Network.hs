{-# LANGUAGE FlexibleContexts #-}

module Network.Network
( Network(..)
, Trainer(..)
, CostFunction
, CostFunction'
, TrainingData

, createNetwork
, predict

--, fit

, quadraticCost
, quadraticCost'
) where

import Network.Neuron
import Network.Layer
import System.Random
import Numeric.LinearAlgebra

-- | Networks are constructed front to back. Start by adding an input layer,
--   then each hidden layer, and finally an output layer.
data Network a = Network { layers :: [Layer a] }

-- | A CostFunction is used for evaluating a network's performance on a given
--   input
type CostFunction a = Matrix a -> Matrix a -> a

-- | A CostFunction' (derivative) is used in backPropagation
type CostFunction' a = Matrix a -> Matrix a -> Matrix a

-- | A tuple of (input, expected output)
type TrainingData a = (Vector a, Vector a)

-- | Trainer is a typeclass for all trainer types - a trainer will take in
--   an instance of itself, a network, a list of training data, and return a
--   new network trained on the data.
class Trainer a where
  train :: (Floating b) => a -> Network b -> [TrainingData b] -> Network b

-- | The createNetwork function takes in a random transform used for weight
--   initialization, a source of entropy, and a list of layer definitions,
--   and returns a network with the weights initialized per the random
--   transform.
createNetwork :: (RandomGen g, Random a, Floating a, Num (Vector a), Container Vector a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
createNetwork t g (layerDef : layerDef' : otherLayerDefs) =
  Network (layer : layers restOfNetwork)
  where layer = createLayer t g layerDef layerDef'
        restOfNetwork = createNetwork t g (layerDef' : otherLayerDefs)

-- | Predict folds over each layer of the network using the input vector as the
--   first value of the accumulator. It operates on whatever network you pass
--   in.
predict :: (Floating a, Num (Vector a), Container Vector a, Product a) => Vector a -> Network a -> Vector a
predict input network = foldl apply input (layers network)

-- | A non-public function used in the fold in predict that applies the
--   activation function and pushes the input through a layer of the
--   network.
apply :: (Floating a, Num (Vector a), Container Vector a, Product a) => Vector a -> Layer a -> Vector a
apply vector layer = mapVector sigma (weights <> vector + bias)
  where sigma = activation (neuron layer)
        weights = weightMatrix layer
        bias = (biasVector layer)

-- | The quadratic cost function (1/2) * sum (y - a) ^ 2
quadraticCost :: (Floating a, Num (Vector a), Container Vector a) => Matrix a -> Matrix a -> a
quadraticCost y a = sumElements $ 0.5 * (y - a) ^ 2

-- | The derivative of the quadratic cost function sum (y - a)
quadraticCost' :: (Floating a, Num (Vector a), Container Vector a) => Matrix a -> Matrix a -> Matrix a
quadraticCost' y a = y - a


-- updateNetwork :: (Floating a) => [(Matrix a, Matrix a)] -> Network a -> Network a
-- updateNetwork [] network = network
-- updateNetwork (update: restOfUpdates) network =
--   Network (updatedLayer : layers restOfUpdatedNetwork)
--   where updatedLayer = updateLayer update (head (layers network))
--         restOfUpdatedNetwork = updateNetwork restOfUpdates restOfNetwork
--         restOfNetwork = Network (drop 1 (layers network))
--
-- updateLayer :: (Floating a) => (Matrix a, Matrix a) -> Layer a -> Layer a
-- updateLayer (weightUpdate, biasUpdate) layer =
--   Layer newWeights newBiases (neuron layer)
--   where newWeights = add (weightMatrix layer) weightUpdate
--         newBiases = add (biasMatrix layer) biasUpdate
--
--
-- -- backprop :: (Floating a, Trainer t) => t -> Network a -> [TrainingData a] -> [(Matrix a, Matrix a)] -> [(Matrix a, Matrix a)]
-- -- backprop trainer network [] updates = updateNetwork updates network
-- -- backprop trainer network (d:ds) updates = backprop trainer network ds (updateLayer updates (deltas network d))
-- -- predictWithState :: (Floating a) => Matrix a -> Network a -> [Matrix a]
-- -- predictWithState input network =
-- --   if null (layers network)
-- --     then [input]
-- --     else input : (predictWithState input' restOfNetwork)
-- --       where input' = feedLayerWithoutActivation input (head (layers network))
-- --             restOfNetwork = Network (tail (layers network))
--
-- -- deltas :: (Floating a, Trainer t) => t -> Network a -> TrainingData a -> [(Matrix a, Matrix a)]
-- -- deltas trainer network trainData = (reverse $ getNablas states (reverse network))
-- --   where states = reverse $ predictWithState (fst trainData) network
--
-- -- deltasOutputs :: (Floating a, Trainer t) => t -> Network a -> TrainingData a -> [Matrix a]
-- -- deltasOutputs trainer network trainData inputs =
-- --   (deltaBias, deltaWeights) : deltasHidden network d (tail inputs)
-- --   where deltaBias = d
-- --         deltaWeights =
-- --         a = predict (fst trainData) network
-- --         y = snd trainData
-- --
--
-- -- parameters
-- --   the network
-- --   delta from l+1 layer
-- --   inputs
-- -- deltasHidden :: (Floating a) => Network a -> Matrix a -> [Matrix a] -> [(Matrix a, Matrix a)]
-- -- deltasHidden network d inputs =
-- --   if null (layers network)
-- --   then []
-- --   else (deltaBias, deltaWeights) : deltasHidden restOfNetwork d' inputs'
-- --   where deltaBias = d'
-- --         deltaWeights = mult outputs d'
-- --         ouputs =
-- --         d' = hadamard (mult weights d) (a' inputs)
-- --         weights = weightMatrix topLayer
-- --         a' = activation' (neuron topLayer)
-- --         topLayer = head (layers network)
-- --         restOfNetwork = Network (tail (layers network))
-- --         inputs' = tail inputs
--
-- -- backprop :: (Floating a, Trainer t) => t -> Network a -> [TrainingData a] -> [(Matrix a, Matrix a)] -> [(Matrix a, Matrix a)]
-- -- backprop trainer network [] updates = updateNetwork updates network
-- -- backprop trainer network (d:ds) updates = backprop trainer network ds (updateLayer updates (deltas t network d))
-- --
-- -- -- feedLayer
-- -- --   feeds an input through one layer
--
-- -- Fits the data for a given number of epochs
-- fit :: (Floating a, Trainer t) => Int ->  t -> Int -> [TrainingData a] -> Network a -> Network a
-- fit 0 t batch trainData network = network
-- fit n t batch trainData network = fit (n - 1) t batch trainData (epoch t batch trainData network)
--
-- -- Runs through all of the data minibatch by minibatch calling the trainer's train function
-- epoch :: (Floating a, Trainer t) => t -> Int -> [TrainingData a] -> Network a -> Network a
-- epoch t batch [] network = network
-- epoch t batch trainData network = epoch t batch tails (train t network miniBatch)
--   where miniBatch = take batch trainData
--         tails = drop batch trainData
--
-- -- Training a network
-- -- data BackpropTrainer a = BackpropTrainer { eta :: a
-- --                                          , cost :: CostFunction a
-- --                                          , cost' :: CostFunction' a
-- --                                          }
-- -- instance (Floating a) => Trainer (BackpropTrainer a) where
-- --   train trainer network trainData = backprop -- something -- trainer network trainData
--

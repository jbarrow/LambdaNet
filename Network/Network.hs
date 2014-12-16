module Network.Network
( Network(..)
, Trainer(..)
-- , BackpropTrainer(..)
, CostFunction
, CostFunction'
, TrainingData

, getXORNetwork

, feedLayer
, createNetwork
--, fit
, predict
--, predictWithState
--, updateNetwork
--, updateLayer
, quadraticCost
, quadraticCost'
--, epoch
) where

import Network.Neuron
import Network.Layer
import Linear
import System.Random

-- Networks are constructed front to back. Start by adding an input layer,
-- then each hidden layer, and finally an output layer.

data Network a = Network { layers :: [Layer a] }

type CostFunction a = Matrix a -> Matrix a -> a
type CostFunction' a = Matrix a -> Matrix a -> Matrix a

type TrainingData a = (Matrix a, Matrix a)

class Trainer a where
  train :: (Floating b) => a -> Network b -> [TrainingData b] -> Network b

-- createNetwork
--   creates a neural network
-- parameters
--   t = transform function (e.g. uniforms, normals)
--   g = random generator (e.g. mkStdGen 4)
--   layers = a list of LayerDefinitions
-- returns
--   a network with layers defined by the list of layer definitions
createNetwork :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> [LayerDefinition a] -> Network a
-- Base Cases
createNetwork t g [] = Network []
createNetwork t g (layerDef : []) = Network []
-- Return a layer ++ the rest of the network
createNetwork t g (layerDef : (layerDef' : otherLayerDefs)) =
  Network (aLayer : layers restOfNetwork)
  where aLayer = createLayer t g layerDef layerDef'
        restOfNetwork = createNetwork t g (layerDef' : otherLayerDefs)

getXORNetwork :: (Floating a) => Network a
getXORNetwork = Network [l, l']
  where l = Layer [[-11.62, 12.88], [10.99, -13.13]] [[-6.06, -7.19]] sigmoidNeuron
        l' = Layer [[13.34], [13.13]] [[-6.56]] sigmoidNeuron


-- Using a scikit-learn-esque naming scheme - predict to classify data
-- and fit to train the network.

-- Predict folds over each layer of the network using the input vector as the
-- first value of the accumulator. (foldl with an applyLayer function)
-- The applyLayer function multiplies the input and the weight matrix
-- and then applies the neuron activation function via a map to the entire output
-- vector.
predict :: (Floating a) => Matrix a -> Network a -> Matrix a
predict input network =
  if null (layers network)
    then input
    else predict input' restOfNetwork
      where input' = feedLayer input (head (layers network))
            restOfNetwork = Network (tail (layers network))

predictWithState :: (Floating a) => Matrix a -> Network a -> [Matrix a]
predictWithState input network =
  if null (layers network)
    then [input]
    else input : (predictWithState input' restOfNetwork)
      where input' = feedLayer input (head (layers network))
            restOfNetwork = Network (tail (layers network))

-- deltas :: (Floating a) => Network a -> [TrainingData a] -> [(Matrix a, Matrix a)]

updateNetwork :: (Floating a) => [(Matrix a, Matrix a)] -> Network a -> Network a
updateNetwork [] network = network
updateNetwork (update: restOfUpdates) network =
  Network (updatedLayer : layers restOfUpdatedNetwork)
  where updatedLayer = updateLayer update (head (layers network))
        restOfUpdatedNetwork = updateNetwork restOfUpdates restOfNetwork
        restOfNetwork = Network (drop 1 (layers network))

updateLayer :: (Floating a) => (Matrix a, Matrix a) -> Layer a -> Layer a
updateLayer (weightUpdate, biasUpdate) layer =
  Layer newWeights newBiases (neuron layer)
  where newWeights = add (weightMatrix layer) weightUpdate
        newBiases = add (biasMatrix layer) biasUpdate


-- backprop :: (Floating a, Trainer t) => t -> Network a -> [TrainingData a] -> [(Matrix a, Matrix a)] -> [(Matrix a, Matrix a)]
-- backprop trainer network [] updates = updateNetwork updates network
-- backprop trainer network (d:ds) updates = backprop trainer network ds (updateLayer updates (deltas network d))
-- predictWithState :: (Floating a) => Matrix a -> Network a -> [Matrix a]
-- predictWithState input network =
--   if null (layers network)
--     then [input]
--     else input : (predictWithState input' restOfNetwork)
--       where input' = feedLayerWithoutActivation input (head (layers network))
--             restOfNetwork = Network (tail (layers network))

-- deltas :: (Floating a, Trainer t) => t -> Network a -> TrainingData a -> [(Matrix a, Matrix a)]
-- deltas trainer network trainData = (reverse $ getNablas states (reverse network))
--   where states = reverse $ predictWithState (fst trainData) network

-- deltasOutputs :: (Floating a, Trainer t) => t -> Network a -> TrainingData a -> [Matrix a]
-- deltasOutputs trainer network trainData inputs =
--   (deltaBias, deltaWeights) : deltasHidden network d (tail inputs)
--   where deltaBias = d
--         deltaWeights =
--         a = predict (fst trainData) network
--         y = snd trainData
--

-- parameters
--   the network
--   delta from l+1 layer
--   inputs
-- deltasHidden :: (Floating a) => Network a -> Matrix a -> [Matrix a] -> [(Matrix a, Matrix a)]
-- deltasHidden network d inputs =
--   if null (layers network)
--   then []
--   else (deltaBias, deltaWeights) : deltasHidden restOfNetwork d' inputs'
--   where deltaBias = d'
--         deltaWeights = mult outputs d'
--         ouputs =
--         d' = hadamard (mult weights d) (a' inputs)
--         weights = weightMatrix topLayer
--         a' = activation' (neuron topLayer)
--         topLayer = head (layers network)
--         restOfNetwork = Network (tail (layers network))
--         inputs' = tail inputs

-- updateNetwork :: (Floating a) => [(Matrix a, Matrix a)] -> Network a -> Network a
-- updateNetwork [] network = network
-- updateNetwork (update: restOfUpdates) network =
--   Network (updatedLayer : layers restOfUpdatedNetwork)
--   where updatedLayer = updateLayer update (head (layers network))
--         restOfUpdatedNetwork = updateNetwork restOfUpdates restOfNetwork
--         restOfNetwork = Network (tail (layers network))
--
-- updateLayer :: (Floating a) => (Matrix a, Matrix a) -> Layer a -> Layer a
-- updateLayer (weightUpdate, biasUpdate) layer =
--   Layer newWeights newBiases (neuron layer)
--   where newWeights = add (weightMatrix layer) weightUpdate
--         newBiases = add (biasMatrix layer) biasUpdate
--
-- backprop :: (Floating a, Trainer t) => t -> Network a -> [TrainingData a] -> [(Matrix a, Matrix a)] -> [(Matrix a, Matrix a)]
-- backprop trainer network [] updates = updateNetwork updates network
-- backprop trainer network (d:ds) updates = backprop trainer network ds (updateLayer updates (deltas t network d))
--
-- -- feedLayer
-- --   feeds an input through one layer

feedLayer :: (Floating a) => Matrix a -> Layer a -> Matrix a
feedLayer input layer = (map . map) a (add z b)
  where a = activation (neuron layer)
        z = mult input w
        b = biasMatrix layer
        w = weightMatrix layer

feedLayerWithoutActivation :: (Floating a) => Matrix a -> Layer a -> Matrix a
feedLayerWithoutActivation input layer = add z b
  where z = mult input w
        b = biasMatrix layer
        w = weightMatrix layer

-- Fits the data for a given number of epochs
fit :: (Floating a, Trainer t) => Int ->  t -> Int -> [TrainingData a] -> Network a -> Network a
fit 0 t batch trainData network = network
fit n t batch trainData network = fit (n - 1) t batch trainData (epoch t batch trainData network)

-- Runs through all of the data minibatch by minibatch calling the trainer's train function
epoch :: (Floating a, Trainer t) => t -> Int -> [TrainingData a] -> Network a -> Network a
epoch t batch [] network = network
epoch t batch trainData network = epoch t batch tails (train t network miniBatch)
  where miniBatch = take batch trainData
        tails = drop batch trainData

-- Training a network
-- data BackpropTrainer a = BackpropTrainer { eta :: a
--                                          , cost :: CostFunction a
--                                          , cost' :: CostFunction' a
--                                          }
-- instance (Floating a) => Trainer (BackpropTrainer a) where
--   train trainer network trainData = backprop -- something -- trainer network trainData

-- So far we provide one defined cost function, the quadratic cost. Eventually
-- we will add more cost functions.
quadraticCost :: (Floating a) => Matrix a -> Matrix a -> a
quadraticCost y a = 0.5 * (sum $ map sum $ (map . map) (^2) $ sub y a)

quadraticCost' :: (Floating a) => Matrix a -> Matrix a -> Matrix a
quadraticCost' y a = sub y a

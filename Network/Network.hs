module Network.Network
( Network(..)
, Trainer(..)
, BackpropTrainer(..)

, feedLayer
, CostFunction
, CostFunction'
, TrainingData

, createNetwork
, fit
, predict
, predictWithState
, quadraticCost
, quadraticCost'
, epoch
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
            restOfNetwork = Network (drop 1 (layers network))

predictWithState :: (Floating a) => Matrix a -> Network a -> [Matrix a]
predictWithState input network =
  if null (layers network)
    then [input]
    else input : (predictWithState input' restOfNetwork)
      where input' = feedLayer input (head (layers network))
            restOfNetwork = Network (drop 1 (layers network))

-- feedLayer
--   feeds an input through one layer
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

data BackpropTrainer a = BackpropTrainer { eta :: a
                                         , cost :: CostFunction a
                                         , cost' :: CostFunction' a
                                         }

instance (Floating a) => Trainer (BackpropTrainer a) where
  train trainer network trainData = network

-- So far we provide one defined cost function, the quadratic cost. Eventually
-- we will add more cost functions.
quadraticCost :: (Floating a) => Matrix a -> Matrix a -> a
quadraticCost y a = 0.5 * (sum $ map sum $ (map . map) (^2) $ sub y a)

quadraticCost' :: (Floating a) => Matrix a -> Matrix a -> Matrix a
quadraticCost' y a = sub y a

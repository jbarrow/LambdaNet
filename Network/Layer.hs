{-# LANGUAGE FlexibleContexts,
             RecordWildCards #-}

module Network.Layer
( LayerDefinition(..)
, Layer(..)
, ShowableLayer(..)
, Connectivity
, RandomTransform

, layerToShowable
, showableToLayer

, createLayer
, connectFully
, randomList
, boxMuller
, normals
, uniforms
) where

import Network.Neuron
import System.Random
import Numeric.LinearAlgebra
import Data.Binary (encode, decode, Binary(..))

-- | The LayerDefinition type is an intermediate type initialized by the
--   library user to define the different layers of the network.
data LayerDefinition a = LayerDefinition { neuronDef :: (Neuron a)
                                         , neuronCount :: Int
                                         , connect :: (Connectivity a)
                                         }

-- | The Layer type, which stores the weight matrix, the bias matrix, and
--   a neuron type.
data Layer a = Layer { weightMatrix :: (Matrix a)
                     , biasVector :: (Vector a)
                     , neuron :: (Neuron a)
                     }

-- | We have to define a new type to be able to serialize and store
--   networks.
data ShowableLayer a = ShowableLayer { weights :: (Matrix a)
                                     , biases :: (Vector a)
                                     } deriving Show

instance (Element a, Binary a) => Binary (ShowableLayer a) where
  put ShowableLayer{..} = do put weights; put biases
  get = do weights <- get; biases <- get; return ShowableLayer{..}

-- | Connectivity is the type alias for a function that defines the connective
--   matrix for two layers (fully connected, convolutionally connected, etc.)
type Connectivity a = Int -> Int -> Matrix a

-- | A random transformation type alias. It is a transformation defined on an
--   infinite list of uniformly distributed random numbers, and returns a list
--   distributed on the transforming distribution.
type RandomTransform a = [a] -> [a]

-- | The createLayer function takes in a random transformation on an infinite
--   stream of uniformly generated numbers, a source of entropy, and two
--   layer definitions, one for the previous layer and one for the next layer.
--   It returns a layer defined by the Layer type -- a weight matrix, a bias
--   vector, and a neuron type.
createLayer ::
  (RandomGen g, Random a, Floating (Vector a), Container Vector a, Floating a)
  => RandomTransform a -> g -> LayerDefinition a -> LayerDefinition a -> Layer a
createLayer t g layerDef layerDef' =
  Layer (randomMatrix * (connectivity i j))
        (randomVector * bias)
        (neuronDef layerDef)
  where randomMatrix = (i >< j) (randomList t g)
        randomVector = i |> (randomList t g)
        i = neuronCount layerDef'
        j = neuronCount layerDef
        connectivity = connect layerDef'
        bias = i |> (repeat 1) -- bias connectivity (full)

-- | The connectFully function takes the number of input neurons for a layer, i,
--   and the number of output neurons of a layer, j, and returns an i x j
--   connectivity matrix for a fully connected network.
connectFully :: Int -> Int -> Matrix Float
connectFully i j = (i >< j) (repeat 1)

-- | We want to be able to convert between layers and showable layers,
--   and vice-versa
layerToShowable :: (Floating (Vector a), Container Vector a, Floating a)
  => Layer a -> ShowableLayer a
layerToShowable l = ShowableLayer (weightMatrix l) (biasVector l)

-- | To go from a showable to a layer, we also need a neuron type,
--   which is an unfortunate restriction owed to Haskell's inability to
--   serialize functions.
showableToLayer :: (Floating (Vector a), Container Vector a, Floating a)
  => (ShowableLayer a, LayerDefinition a) -> Layer a
showableToLayer (s, d) = Layer (weights s) (biases s) (neuronDef d)

-- | Initialize an infinite random list given a random transform and a source
--   of entroy.
randomList :: (RandomGen g, Random a, Floating a)
  => RandomTransform a -> g -> [a]
randomList transform = transform . randoms

-- | Define a transformation on the uniform distribution to generate
--   normally distributed numbers in Haskell (the Box-Muller transform)
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller x1 x2 = (z1, z2) where z1 = sqrt ((-2) * log x1) * cos (2 * pi * x2)
                                 z2 = sqrt ((-2) * log x1) * sin (2 * pi * x2)

-- | This is a function of type RandomTransform that transforms a list of
--   uniformly distributed numbers to a list of normally distributed numbers.
normals :: Floating a => [a] -> [a]
normals (x1:x2:xs) = z1:z2:(normals xs) where (z1, z2) = boxMuller x1 x2
normals _ = []

-- | A non-transformation to return a list of uniformly distributed numbers
--   from a list of uniformly distributed numbers. It's really a matter of
--   naming consistency.
uniforms :: Floating a => [a] -> [a]
uniforms xs = xs

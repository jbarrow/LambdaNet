{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Network.Layer
( LayerDefinition(..)
, Layer(..)
, Connectivity
, RandomTransform

, showableToLayer

, createLayer
, scaleLayer
, connectFully

, randomList
, boxMuller
, normals
, uniforms
, boundedUniforms
) where

import           Data.Binary           (Binary (..), decode, encode)
import           Network.Neuron
import           Numeric.LinearAlgebra
import           System.Random

-- | The LayerDefinition type is an intermediate type initialized by the
--   library user to define the different layers of the network.
data LayerDefinition = LayerDefinition { neuronDef   :: Neuron
                                       , neuronCount :: Int
                                       , connect     :: Connectivity
                                       }

-- | The Layer type, which stores the weight matrix, the bias matrix, and
--   a neuron type.
data Layer = Layer { weightMatrix :: Matrix Double
                   , biasVector   :: Vector Double
                   , neuron       :: Neuron
                   } deriving Show

instance Binary (Layer) where
  put Layer{..} = do put weightMatrix; put biasVector
  get = do weightMatrix <- get; biasVector <- get; return Layer{..}

-- | Connectivity is the type alias for a function that defines the connective
--   matrix for two layers (fully connected, convolutionally connected, etc.)
type Connectivity = Int -> Int -> Matrix Double

-- | A random transformation type alias. It is a transformation defined on an
--   infinite list of uniformly distributed random numbers, and returns a list
--   distributed on the transforming distribution.
type RandomTransform = [Double] -> [Double]

-- | The createLayer function takes in a random transformation on an infinite
--   stream of uniformly generated numbers, a source of entropy, and two
--   layer definitions, one for the previous layer and one for the next layer.
--   It returns a layer defined by the Layer type -- a weight matrix, a bias
--   vector, and a neuron type.
createLayer :: (RandomGen g)
  => RandomTransform -> g -> LayerDefinition -> LayerDefinition -> Layer
createLayer t g layerDef layerDef' =
  Layer (randomMatrix * (connectivity i j))
        (randomVector * bias)
        (neuronDef layerDef)
  where randomMatrix = (i >< j) (randomList t g')
        randomVector = i |> (randomList t g'')
        i = neuronCount layerDef'
        j = neuronCount layerDef
        connectivity = connect layerDef'
        bias = i |> (repeat 1) -- bias connectivity (full)
        (g', g'') = split g

scaleLayer :: Double -> Layer -> Layer
scaleLayer factor l =
  Layer (factor `scale` (weightMatrix l)) (factor `scale` (biasVector l)) (neuron l)

-- | The connectFully function takes the number of input neurons for a layer, i,
--   and the number of output neurons of a layer, j, and returns an i x j
--   connectivity matrix for a fully connected network.
connectFully :: Int -> Int -> Matrix Double
connectFully i j = (i >< j) (repeat 1)

-- | To go from a showable to a layer, we also need a neuron type,
--   which is an unfortunate restriction owed to Haskell's inability to
--   serialize functions.
showableToLayer :: (Layer, LayerDefinition) -> Layer
showableToLayer (s, d) = Layer (weightMatrix s) (biasVector s) (neuronDef d)

-- | Initialize an infinite random list given a random transform and a source
--   of entroy.
randomList :: RandomGen g => RandomTransform -> g -> [Double]
randomList transform = transform . randoms

-- | Define a transformation on the uniform distribution to generate
--   normally distributed numbers in Haskell (the Box-Muller transform)
boxMuller :: Double -> Double -> (Double, Double)
boxMuller x1 x2 = (z1, z2)
  where z1 = sqrt ((-2) * log x1) * cos (2 * pi * x2)
        z2 = sqrt ((-2) * log x1) * sin (2 * pi * x2)

-- | This is a function of type RandomTransform that transforms a list of
--   uniformly distributed numbers to a list of normally distributed numbers.
normals :: [Double] -> [Double]
normals (x1:x2:xs) = z1:z2:(normals xs)
  where (z1, z2) = boxMuller x1 x2
normals _ = []

-- | A non-transformation to return a list of uniformly distributed numbers
--   from a list of uniformly distributed numbers. It's really a matter of
--   naming consistency. It generates numbers on the range (0, 1]
uniforms :: [Double] -> [Double]
uniforms xs = xs

-- | An affine transformation to return a list of uniforms on the range
--   (a, b]
boundedUniforms :: (Double, Double) -> [Double] -> [Double]
boundedUniforms (lower, upper) xs = map affine xs
  where affine x = lower + x * (upper - lower)

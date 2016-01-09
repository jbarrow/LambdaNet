{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module AI.Layer
( LayerDefinition(..)
, Layer(..)
, Connectivity
, RandomTransform
, Randomization

, showableToLayer

, createLayer
, scaleLayer
, randomizeFully
, randomizeLocally
, connectFully
, connectLocally

, randomList
, boxMuller
, normals
, uniforms
, boundedUniforms
) where

import           Data.Binary           (Binary (..), decode, encode)
import           AI.Neuron
import           Numeric.LinearAlgebra
import           System.Random

-- | The LayerDefinition type is an intermediate type initialized by the
--   library user to define the different layers of the network.
data LayerDefinition g = LayerDefinition { neuronDef   :: Neuron
                                         , neuronCount :: Int
                                         , connect     :: Connectivity
                                         , randomize   :: Randomization g
                                         }

-- | The Layer type, which stores the weight matrix, the bias matrix, and
--   a neuron type.
data Layer = Layer { weightMatrix :: Matrix Double
                   , biasVector   :: Vector Double
                   , neuron       :: Neuron
                   } deriving Show

instance Binary Layer where
  put Layer{..} = do put weightMatrix; put biasVector
  get = do weightMatrix <- get; biasVector <- get; return Layer{..}

-- | Connectivity is the type alias for a function that defines the connective
--   matrix for two layers (fully connected, convolutionally connected, etc.)
--   and takes in the number of output and input neurons
type Connectivity = Int -> Int -> Matrix Double

-- | Randomiation is the type alias for a function that defines
--   the initial random values for the weight matrix and bias vector
--   for two layers and takes in a random transformation on an infinite
--   stream of uniformly generated numbers, a source of entropy,
--   the number of output neurons, and the number of input neurons
type Randomization g = g -> RandomTransform -> Int -> Int -> (Matrix Double, Vector Double)

-- | ConvolutionalSettings is a type alias for the receptive field size,
--   stride, zero-padding, the number of filters, the number of dimensions,
--   the weight and height of the input and output fields
type ConvolutionalSettings = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int

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
  => RandomTransform -> g -> LayerDefinition g -> LayerDefinition g -> Layer
createLayer t g layerDef layerDef' =
  Layer (randomMatrix * connectivity i j)
        (randomVector * bias)
        (neuronDef layerDef)
  where (randomMatrix, randomVector) = randomize layerDef' g t i j
        i = neuronCount layerDef'
        j = neuronCount layerDef
        connectivity = connect layerDef'
        bias = i |> repeat 1 -- bias connectivity (full)

scaleLayer :: Double -> Layer -> Layer
scaleLayer factor l =
  Layer (factor `scale` weightMatrix l) (factor `scale` biasVector l) (neuron l)

-- | The randomizeFully function takes in a source of entropy, the number of output
--   neurons, and the number of input neurons, and returns a tuple of the 
--   a fully random matrix, and a fully random vector
randomizeFully :: (RandomGen g) => Randomization g
randomizeFully g t i j = (randomMatrix, randomVector)
  where randomMatrix = (i >< j) (randomList t g')
        randomVector = i |> randomList t g''
        (g', g'') = split g

-- | The randomizeLocally function takes in ConvolutionalSettings a source
--   of entropy, a random transform, the number of output and input neurons.
--   It returns a tuple with a random matrix and a random vector
randomizeLocally :: (RandomGen g) => Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Randomization g
randomizeLocally f s p k d w1 h1 w2 h2 g t i j = (randomMatrix, randomVector)
  where randomMatrix = fromLists (locallyRandomList f s p k d w1 h1 w2 h2 g' t i j) :: Matrix Double
        randomVector = i |> randomList t g''
        (g', g'') = split g

-- | The locallyRandomList function takes in ConvolutionalSettings a source
--   of entropy, a random transform, the number of output and input neurons.
--   It returns a list of lists of locally random numbers
locallyRandomList :: (RandomGen g) => Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> g -> RandomTransform -> Int -> Int -> [[Double]]
locallyRandomList f s p k d w1 h1 w2 h2 g t i j =
  if k == 0 then []
  else filterValues ++ nextFilterValues
    where filterValues = [replicate (rowZeroOffset + colZeroOffset) 0
                          ++ take (j - rowZeroOffset - colZeroOffset) (randomList t g')
                          | n <- [0..div i k-1],
                          let rowSize = w1 + 2 * p,
                          let postsynPerFilter = rem n (div i k),
                          let rowZeroOffset = (1 + s) * quot postsynPerFilter w2 * rowSize,
                          let colZeroOffset = (1 + s) * mod n w2]
          nextFilterValues = locallyRandomList f s p (k - 1) k w1 h1 w2 h2 g'' t (i - div i k) j
          (g', g'') = split g

-- | The connectFully function takes the number of input neurons for a layer, i,
--   and the number of output neurons of a layer, j, and returns an i x j
--   connectivity matrix for a fully connected network.
connectFully :: Connectivity
connectFully i j = (i >< j) (repeat 1)

-- | The connectLocally function takes in ConvolutionalSettings and the number
--   of output and input neurons
connectLocally :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Connectivity
connectLocally f s p k d w1 h1 w2 h2 i j =
  repmat (fromLists conn :: Matrix Double) k d
  where conn = [replicate rowZeroOffset 0
                ++ take (f * rowSize) (cycle fieldArea)
                ++ replicate (rowSize * colSize - rowSize * f - rowZeroOffset) 0
                | n <- [0.. quot i k-1],
                  let rowSize = w1 + 2 * p,
                  let colSize = h1 + 2 * p,
                  let rowZeroOffset = (1 + s) * quot n w2 * rowSize,
                  let fieldAreaZeroOffset = (1 + s) * mod n w2,
                  let fieldArea = replicate fieldAreaZeroOffset 0
                                   ++ replicate f 1
                                   ++ replicate (rowSize - f - fieldAreaZeroOffset) 0]

-- | To go from a showable to a layer, we also need a neuron type,
--   which is an unfortunate restriction owed to Haskell's inability to
--   serialize functions.
showableToLayer :: (RandomGen g) => (Layer, LayerDefinition g) -> Layer
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
normals :: RandomTransform
normals (x1:x2:xs) = z1:z2:normals xs
  where (z1, z2) = boxMuller x1 x2
normals _ = []

-- | A non-transformation to return a list of uniformly distributed numbers
--   from a list of uniformly distributed numbers. It's really a matter of
--   naming consistency. It generates numbers on the range (0, 1]
uniforms :: RandomTransform
uniforms xs = xs

-- | An affine transformation to return a list of uniforms on the range
--   (a, b]
boundedUniforms :: (Double, Double) -> [Double] -> [Double]
boundedUniforms (lower, upper) = map affine
  where affine x = lower + x * (upper - lower)

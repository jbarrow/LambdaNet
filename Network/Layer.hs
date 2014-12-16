module Network.Layer
( LayerDefinition(..)
, Layer(..)
, Connectivity

, createLayer
, connectFully
) where

import Network.Neuron
import Linear
import System.Random

data LayerDefinition a = LayerDefinition { neuronDef :: (Neuron a)
                                         , neuronCount :: Int
                                         , connect :: (Connectivity a)
                                         } --deriving (Generic)

--instance FromJSON LayerDefinition
--instance ToJSON LayerDefinition

data Layer a = Layer { weightMatrix :: (Matrix a)
                     , biasMatrix :: (Matrix a)
                     , neuron :: (Neuron a)
                     }

type Connectivity a = Int -> Int -> Matrix a

-- createLayer
--   creates a layer from two layer definitions
-- parameters
--   t = transform function (e.g. uniforms, normals)
--   g = random generator (e.g. mkStdGen 4)
--   layerDef = LayerDefinition
--   layerDef' = another LayerDefinition
-- returns
--   a layer defined by layerDef
--   with connections from layerDef to layerDef'
createLayer :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> LayerDefinition a -> LayerDefinition a -> Layer a
createLayer t g layerDef layerDef' =
  Layer (hadamard randomMatrix (connectivity i j))
        (hadamard randomMatrix' (connectivity i j))
        (neuronDef layerDef)
  where randomMatrix = reshape j (take (i*j) (randomList t g))
        randomMatrix' = [take j (randomList t g)]
        i = neuronCount layerDef
        j = neuronCount layerDef'
        connectivity = connect layerDef

-- connectFully
--   a connectivity function that fully connects input neurons
--   to the output neurons
-- parameters
--   i = the count of input neurons for a layer
--   j = the count of the output neurons for a layer
-- returns
--   a i by j connectivity matrix
--   elements of matrix in {0, 1}
--   (or it could return boolean values)
connectFully :: Num a => Int -> Int -> Matrix a
connectFully i j = take i (repeat (take j (repeat 1)))

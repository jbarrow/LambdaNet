module AI.DemoNeuron
       ( Neuron(..)
       , SigmoidNeuron(..)
       , TanhNeuron(..)
       , RecluNeuron(..)
       , L2Neuron(..)

       , NeuronWeights
       , Values

       , sigmoid, sigmoid'
       , tanh, tanh'
       , reclu, reclu'
       , l1Norm, l2Norm
       ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Char8 as C

data SigmoidNeuron = SigmoidNeuron deriving (Show)
data TanhNeuron = TanhNeuron deriving (Show)
data RecluNeuron = RecluNeuron deriving (Show)
data L2Neuron = L2Neuron deriving (Show)

type NeuronWeights = Vector Double
type Values = Vector Double
type Activation = Double

-- | A Neuron type has two functions -- evaluate and evaluate',
--   both of which are functions from NeuronWeights to input values
--   to doubles.
class (Show a, Binary a) => Neuron a where
  evaluate :: a -> NeuronWeights -> Values -> Activation
  evaluate' :: a -> NeuronWeights -> Values -> Activation

-- | Declare the Sigmoid Neuron to be a binary type
instance Binary (SigmoidNeuron) where
  put SigmoidNeuron = putByteString (C.pack (show SigmoidNeuron))
  get = do return SigmoidNeuron

-- | Declare the Tanh neuron to be a binary type
instance Binary (TanhNeuron) where
  put TanhNeuron = putByteString (C.pack (show TanhNeuron))
  get = do return TanhNeuron

-- | Declare a Rectified Linear Neuron to be a binary type
instance Binary (RecluNeuron) where
  put RecluNeuron = putByteString (C.pack (show RecluNeuron))
  get = do return RecluNeuron

-- | Declare an L2 Neuron to be a binary type
instance Binary (L2Neuron) where
  put L2Neuron = putByteString (C.pack (show L2Neuron))
  get = do return L2Neuron

instance Neuron (SigmoidNeuron) where
  evaluate  n w v = sigmoid  $ l1Norm w v
  evaluate' n w v = sigmoid' $ l1Norm w v

instance Neuron (TanhNeuron) where
  evaluate  n w v = tanh  $ l1Norm w v
  evaluate' n w v= tanh' $ l1Norm w v

instance Neuron (RecluNeuron) where
  evaluate  n w v = reclu  $ l1Norm w v
  evaluate' n w v = reclu' $ l1Norm w v

instance Neuron (L2Neuron) where
  evaluate  n = l2Norm
  evaluate' n = l1Norm

-- | Compute a dot product, but ensure that the dimensions of both
--   vectors are the same size.
l1Norm :: NeuronWeights -> Values -> Double
l1Norm w v = if size w /= size v
             then error "Neuron NeuronWeights and values don't align"
             else dot w v

-- | The sigmoid activation function, a standard activation function defined
--   on the range (0, 1).
sigmoid :: Double -> Activation
sigmoid t = 1 / (1 + exp (-1 * t))

-- | The derivative of the sigmoid function conveniently can be computed in
--   terms of the sigmoid function.
sigmoid' :: Double -> Activation
sigmoid' t = s * (1 - s)
  where s = sigmoid t

-- | The hyperbolic tangent activation function is provided in Prelude. Here
--   we provide the derivative. As with the sigmoid function, the derivative
--   of tanh can be computed in terms of tanh.
tanh' :: Double -> Activation
tanh' t = 1 - s ^ 2
  where s = tanh t

-- | The rectified linear activation function. This is a more "biologically
--   accurate" activation function that still retains differentiability.
reclu :: Double -> Activation
reclu t = log (1 + exp t)

-- | The derivative of the rectified linear activation function is just the
--   sigmoid.
reclu' :: Double -> Activation
reclu' t = sigmoid t

-- | Calculate the distance between a SOM neuron and an input
l2Norm :: NeuronWeights -> Values -> Activation
l2Norm a b = sqrt $ sum $ map (^2) $ zipWith (-) (toList a) (toList b)

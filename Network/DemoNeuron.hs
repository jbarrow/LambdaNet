module Network.Neuron
       ( Neuron(..)
       , SigmoidNeuron(..)
       , TanhNeuron(..)
       , RecluNeuron(..)
       , L2Neuron(..)

       , Weights
       , Values

       , sigmoid, sigmoid'
       , tanh, tanh'
       , reclu, reclu'
       , distance
       ) where

import Numeric.LinearAlgebra

data SigmoidNeuron = SigmoidNeuron
data TanhNeuron = TanhNeuron
data RecluNeuron = RecluNeuron
data L2Neuron = L2Neuron

type Weights = Vector Double
type Values = Vector Double

-- | A Neuron type has two functions -- evaluate and evaluate',
--   both of which are functions from weights to input values
--   to doubles.
class (Show a) => Neuron a where
  evaluate :: a -> Weights -> Values -> Double
  evaluate' :: a -> Weights -> Values -> Double

instance Show (SigmoidNeuron) where
  show n = "Sigmoid Neuron"

instance Show (TanhNeuron) where
  show n = "Hyperbolic Tangent Neuron"

instance Show (RecluNeuron) where
  show n = "Rectified Linear Neuron"
  
instance Show (L2Neuron) where
  show n = "Euclidean Distance Neuron"

instance Neuron (SigmoidNeuron) where
  evaluate  n w v = sigmoid  $ dot w v
  evaluate' n w v = sigmoid' $ dot w v

instance Neuron (TanhNeuron) where
  evaluate  n w v = tanh  $ dot w v
  evaluate' n w v= tanh' $ dot w v

instance Neuron (RecluNeuron) where
  evaluate  n w v = reclu  $ dot w v
  evaluate' n w v = reclu' $ dot w v

instance Neuron (L2Neuron) where
  evaluate  n = distance
  evaluate' n = dot

-- | The sigmoid activation function, a standard activation function defined
--   on the range (0, 1).
sigmoid :: Double -> Double
sigmoid t = 1 / (1 + exp (-1 * t))

-- | The derivative of the sigmoid function conveniently can be computed in
--   terms of the sigmoid function.
sigmoid' :: Double -> Double
sigmoid' t = s * (1 - s)
  where s = sigmoid t

-- | The hyperbolic tangent activation function is provided in Prelude. Here
--   we provide the derivative. As with the sigmoid function, the derivative
--   of tanh can be computed in terms of tanh.
tanh' :: Double -> Double
tanh' t = 1 - s ^ 2
  where s = tanh t

-- | The rectified linear activation function. This is a more "biologically
--   accurate" activation function that still retains differentiability.
reclu :: Double -> Double
reclu t = log (1 + exp t)

-- | The derivative of the rectified linear activation function is just the
--   sigmoid.
reclu' :: Double -> Double
reclu' t = sigmoid t

-- | Calculate the distance between a SOM neuron and an input
distance :: Vector Double -> Vector Double -> Double
distance a b = sqrt $ sum $ map (^2) $ zipWith (-) (toList a) (toList b)


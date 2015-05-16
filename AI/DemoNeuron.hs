module AI.DemoNeuron
       ( Neuron(..)
       , L2Neuron(..)
       , ReducedNeuron(..)

       , NeuronWeights
       , Values
       , ActivationFunction
       , ActivationFunction'

       , sigmoidNeuron
       , tanhNeuron
       , recluNeuron
         
       , sigmoid, sigmoid'
       , tanh, tanh'
       , reclu, reclu'
       , l1Norm, l2Norm
       ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

type ActivationFunction = Double -> Double
type ActivationFunction' = Double -> Double

data L2Neuron = L2Neuron deriving (Show)
data ReducedNeuron = ReducedNeuron { activation :: ActivationFunction
                                   , activation' :: ActivationFunction'
                                   , description :: String
                                   }

type NeuronWeights = Vector Double
type Values = Vector Double
type Activation = Double

-- | A Neuron type has two functions -- evaluate and evaluate',
--   both of which are functions from NeuronWeights to input values
--   to doubles.
class (Show a) => Neuron a where
  evaluate :: a -> NeuronWeights -> Values -> Activation
  evaluate' :: a -> NeuronWeights -> Values -> Activation

instance Show (ReducedNeuron) where
  show n = description n

instance Neuron (ReducedNeuron) where
    evaluate n weights values = f $ dot weights values
        where f = activation n

    evaluate' n weights values = f' $ dot weights values
        where f' = activation' n

           
instance Neuron (L2Neuron) where
  evaluate  n = l2Norm
  evaluate' n = l1Norm

-- | Our provided neuron types: sigmoid, tanh, reclu
sigmoidNeuron :: ReducedNeuron
sigmoidNeuron = ReducedNeuron sigmoid sigmoid' "sigmoid"

tanhNeuron :: ReducedNeuron
tanhNeuron = ReducedNeuron tanh tanh' "tanh"

recluNeuron :: ReducedNeuron
recluNeuron = ReducedNeuron reclu reclu' "reclu"

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

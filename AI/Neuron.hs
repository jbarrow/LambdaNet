module AI.Neuron
       ( Neuron(..)

       , ActivationFunction
       , ActivationFunction'
       , sigmoidNeuron
       , tanhNeuron
       , recluNeuron
       , l2Neuron

       , sigmoid
       , sigmoid'
       , tanh
       , tanh'
       , reclu
       , reclu'
       ) where

-- | Using this structure allows users of the library to create their own
--   neurons by creating two functions - an activation function and its
--   derivative - and packaging them up into a neuron type.
data Neuron = Neuron { activation  :: ActivationFunction
                     , activation' :: ActivationFunction'
                     , description :: String
                     }

instance Show (Neuron) where
  show n = description n

type ActivationFunction = Double -> Double
type ActivationFunction' = Double -> Double

-- | Our provided neuron types: sigmoid, tanh, reclu
sigmoidNeuron :: Neuron
sigmoidNeuron = Neuron sigmoid sigmoid' "sigmoid"

tanhNeuron :: Neuron
tanhNeuron = Neuron tanh tanh' "tanh"

recluNeuron :: Neuron
recluNeuron = Neuron reclu reclu' "reclu"

l2Neuron :: Neuron
l2Neuron = Neuron (^2) id "L2"

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

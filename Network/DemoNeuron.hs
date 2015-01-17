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


data SigmoidNeuron = SigmoidNeuron
data TanhNeuron = TanhNeuron
data RecluNeuron = RecluNeuron
data L2Neuron = L2Neuron

type Weights = Vector Double
type Values = Vector Double

class (Show a) => Neuron a where
  evaluate :: Weights -> Values -> Double
  evaluate' :: Weights -> Values -> Double

instance Show (SigmoidNeuron) where
  show = "Sigmoid Neuron"

instance Show (TanhNeuron) where
  show = "Hyperbolic Tangent Neuron"

instance Show (RecluNeuron) where
  show = "Rectified Linear Neuron"

instance Show (L2Neuron) where
  show = "Euclidean Distance Neuron"

instance Neuron (SigmoidNeuron) where
  evaluate  = sigmoid  . dot
  evaluate' = sigmoid' . dot

instance Neuron (TanhNeuron) where
  evaluate  = tanh  . dot
  evaluate' = tanh' . dot

instance Neuron (RecluNeuron) where
  evaluate  = reclu  . dot
  evaluate' = reclu' . dot

instance Neuron (L2Neuron) where
  evaluate  = distance
  evaluate' = dot

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


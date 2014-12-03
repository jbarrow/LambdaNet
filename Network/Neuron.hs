module Network.Neuron
( Neuron(..)

, ActivationFunction
, ActivationFunction'
, sigmoid_neuron
, tanh_neuron
, reclu_neuron

, evaluate
, evaluate'

, sigmoid
, sigmoid'
, logistic
, logistic'
, reclu
, reclu'
) where

-- Using this structure allows users of the library to create their own
-- neurons by creating two functions - an activation function and its
-- derivative - and packaging them up into a neuron type.
data Neuron a = Neuron (ActivationFunction a) (ActivationFunction' a)

type ActivationFunction a = a -> a
type ActivationFunction' a = a -> a

-- Evaluate the activation of a neuron by pattern matching it on
-- the constructor.
evaluate :: (Floating a) => Neuron a -> [a] -> a
evaluate (Neuron f f') = f . sum

-- Evaluate the derivative of a neuron, given a floating point vector
evaluate' :: (Floating a) => Neuron a -> [a] -> a
evaluate' (Neuron f f') = f' . sum

-- Our provided neuron types: sigmoid, tanh, reclu
sigmoid_neuron :: (Floating a) => Neuron a
sigmoid_neuron = Neuron sigmoid sigmoid'

tanh_neuron :: (Floating a) => Neuron a
tanh_neuron = Neuron logistic logistic'

reclu_neuron :: (Floating a) => Neuron a
reclu_neuron = Neuron reclu reclu'

-- The sigmoid activation function, a standard activation function defined
-- on the range (0, 1).
sigmoid :: (Floating a) => a -> a
sigmoid t = 1 / (1 + exp (-1 * t))

-- The derivative of the sigmoid function conveniently can be computed in terms
-- of the sigmoid function.
sigmoid' :: (Floating a) => a -> a
sigmoid' t = s * (1 - s)
              where s = sigmoid t

-- The hyperbolic tangent activation function, another logistic function,
-- but unlike sigmoid it is defined on the range (-1, 1). Unfortunately,
-- we could not call it tanh due to naming conflicts with the Prelude tanh.
logistic :: (Floating a) => a -> a
logistic t = (1 - exp (-2 * t)) / (1 + exp (-2 * t))

-- As with the sigmoid function, the derivative of tanh can be computed in
-- terms of tanh.
logistic' :: (Floating a) => a -> a
logistic' t = 1 - s ^ 2
               where s = logistic t

-- The rectified linear activation function. This is a more "biologically
-- accurate" activation function that still retains differentiability.
reclu :: (Floating a) => a -> a
reclu t = log (1 + exp t)

-- The derivative of the rectified linear activation function is just the
-- sigmoid.
reclu' :: (Floating a) => a -> a
reclu' t = sigmoid t

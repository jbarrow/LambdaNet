module Network
( sigmoid
, logistic
, reclu
) where

-- The sigmoid activation function
sigmoid :: (Floating a) => a -> a
sigmoid t = 1 / (1 + exp (-1 * t))

-- The hyperbolic tangent activation function
logistic :: (Floating a) => a -> a
logistic t = (1 - exp (-2 * t)) / (1 + exp (-2 * t))

-- The rectified linear activation function
reclu :: (Floating a) => a -> a
reclu t = log (1 + exp t)

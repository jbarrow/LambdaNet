{-# LANGUAGE FlexibleContexts #-}

module Network.Trainer
( BackpropTrainer(..)
, CostFunction
, CostFunction'

, quadraticCost
, quadraticCost'
, backprop
) where

import Network.Network
import Network.Neuron
import Network.Layer
import System.Random
import Numeric.LinearAlgebra

-- | A BackpropTrainer performs simple backpropagation on a neural network.
--   It can be used as the basis for more complex trainers.
data BackpropTrainer a = BackpropTrainer { eta :: a
                                         , cost :: CostFunction a
                                         , cost' :: CostFunction' a
                                         }

-- | A CostFunction is used for evaluating a network's performance on a given
--   input
type CostFunction a = Matrix a -> Matrix a -> a

-- | A CostFunction' (derivative) is used in backPropagation
type CostFunction' a = Matrix a -> Matrix a -> Matrix a

-- | The quadratic cost function (1/2) * sum (y - a) ^ 2
quadraticCost :: (Floating a, Num (Vector a), Container Vector a) => Matrix a -> Matrix a -> a
quadraticCost y a = sumElements $ 0.5 * (y - a) ^ 2

-- | The derivative of the quadratic cost function sum (y - a)
quadraticCost' :: (Floating a, Num (Vector a), Container Vector a) => Matrix a -> Matrix a -> Matrix a
quadraticCost' y a = y - a

-- | Declare the BackpropTrainer to be an instance of Trainer.
instance (Floating a) => Trainer (BackpropTrainer a) where
  fit t n [] = n
  fit t n trainData = fit t (backprop t n (head trainData)) (tail trainData)

-- | Perform backpropagation on a single training data instance.
backprop :: (Floating a) => BackpropTrainer a -> Network a -> TrainingData a -> Network a
backprop t n trainData = n

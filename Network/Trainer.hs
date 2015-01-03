{-# LANGUAGE FlexibleContexts, InstanceSigs #-}

module Network.Trainer
( Trainer(..)
, CostFunction
, CostFunction'
, TrainingData
, Selection

, quadraticCost
, quadraticCost'
, minibatch
, online
) where

import Network.Network
import Network.Neuron
import Network.Layer

import System.Random
import System.Random.Shuffle (shuffle')
import Data.List.Split (chunksOf)
import Numeric.LinearAlgebra

-- | Trainer is a typeclass for all trainer types - a trainer will take in
--   an instance of itself, a network, a list of training data, and return a
--   new network trained on the data.
class Trainer a where
  fit :: Selection -> a -> Network -> [TrainingData] -> Network

-- | A CostFunction is used for evaluating a network's performance on a given
--   input
type CostFunction = Vector Double -> Vector Double -> Double

-- | A CostFunction' (derivative) is used in backPropagation
type CostFunction' = Vector Double -> Vector Double -> Vector Double

-- | A tuple of (input, expected output)
type TrainingData = (Vector Double, Vector Double)

-- | A selection function for performing gradient descent
type Selection = [TrainingData] -> [[TrainingData]]

-- | The quadratic cost function (1/2) * sum (y - a) ^ 2
quadraticCost :: Vector Double -> Vector Double -> Double
quadraticCost y a = sumElements $ 0.5 * (a - y) ** 2

-- | The derivative of the quadratic cost function sum (y - a)
quadraticCost' :: Vector Double -> Vector Double -> Vector Double
quadraticCost' y a = a - y

-- | The minibatch function becomes a Selection when partially applied
--   with the minibatch size
minibatch :: Int -> [TrainingData] -> [[TrainingData]]
minibatch size = chunksOf size

-- | If we want to train the network online
online :: [TrainingData] -> [[TrainingData]]
online = minibatch 1

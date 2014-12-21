{-# LANGUAGE FlexibleContexts #-}

import Network.Neuron
import Network.Layer
import Network.Network
import Network.Trainer
import System.IO
import System.Random
import Numeric.LinearAlgebra

trainNtimes :: (Floating a, Num (Vector a), Container Vector a, Product a)
  => Network a -> Int -> BackpropTrainer a -> Network a
trainNtimes n 0 t = n
trainNtimes n c t = trainNtimes (fit t n [(fromList [0.0, 1.0], fromList [1.0]), (fromList [1.0, 1.0], fromList [0.0]), (fromList [1.0, 0.0], fromList [1.0]), (fromList [0.0, 0.0], fromList [0.0])]) (c - 1) t

main = do
  let l = LayerDefinition sigmoidNeuron 2 connectFully
  let l' = LayerDefinition sigmoidNeuron 2 connectFully
  let l'' = LayerDefinition sigmoidNeuron 1 connectFully
  let g = mkStdGen 4
  let n = createNetwork normals g [l, l', l'']
  let t = BackpropTrainer 0.3 quadraticCost quadraticCost'
  print $ predict (fromList [0.0, 1.0]) n
  print $ length $ layers n
  let n' = trainNtimes n 100 t
  print $ (weightMatrix (head $ layers n'))
  print $ (weightMatrix (last $ layers n'))
  print $ predict (fromList [0.0, 1.0]) n'

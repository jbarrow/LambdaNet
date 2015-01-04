{-# LANGUAGE FlexibleContexts #-}

import Network.Neuron
import Network.Layer
import Network.Network
import Network.Trainer
import Network.Trainer.BackpropTrainer

import Network.Visualizations
import System.IO
import System.Random
import Numeric.LinearAlgebra

main :: IO ()
main = do
  let l = LayerDefinition sigmoidNeuron 2 connectFully
  let l' = LayerDefinition sigmoidNeuron 2 connectFully
  let l'' = LayerDefinition sigmoidNeuron 1 connectFully

  g <- newStdGen
  let n = createNetwork normals g [l, l', l'']

  let t = BackpropTrainer (3 :: Double) quadraticCost quadraticCost'
  let dat = [(fromList [0, 1], fromList [1]), (fromList [1, 1], fromList [0]), (fromList [1, 0], fromList [1]), (fromList [0, 0], fromList [0])]

  let n' = trainNTimes g n t online dat 1000

  putStrLn "==> XOR predictions: "
  print $ predict (fromList [0, 0]) n'
  print $ predict (fromList [1, 0]) n'
  print $ predict (fromList [0, 1]) n'
  print $ predict (fromList [1, 1]) n'

  saveNetwork "xor.ann" n'

  putStrLn $ "==> Network saved and reloaded: "
  n'' <- loadNetwork "xor.ann" [l, l', l'']

  print $ predict (fromList [0, 0]) n''
  print $ predict (fromList [1, 0]) n''
  print $ predict (fromList [0, 1]) n''
  print $ predict (fromList [1, 1]) n''

  networkHistogram "weights.png" weightList n''
  networkHistogram "biases.png" biasList n''

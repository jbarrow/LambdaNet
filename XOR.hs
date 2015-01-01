{-# LANGUAGE FlexibleContexts #-}

import Network.Neuron
import Network.Layer
import Network.Network
import Network.Trainer
import System.IO
import System.Random
import Numeric.LinearAlgebra

-- trainNtimes :: (Floating (Vector a), Container Vector a, Product a)
--   => Network a -> Int -> BackpropTrainer a -> Network a
-- trainNtimes n 0 t = n
-- trainNtimes n c t = trainNtimes (fit online t n
--   [(fromList [0, 1], fromList [1]),
--    (fromList [1, 1], fromList [0]),
--    (fromList [1, 0], fromList [1]),
--    (fromList [0, 0], fromList [0])]) (c - 1) t

main :: IO ()
main = do
  let l = LayerDefinition sigmoidNeuron 2 connectFully
  let l' = LayerDefinition sigmoidNeuron 2 connectFully
  let l'' = LayerDefinition sigmoidNeuron 1 connectFully

  let n = createNetwork normals (mkStdGen 4) [l, l', l'']

  let t = BackpropTrainer (3 :: Float) quadraticCost quadraticCost'
  let dat = [(fromList [0, 1], fromList [1]),
             (fromList [1, 1], fromList [0]),
             (fromList [1, 0], fromList [1]),
             (fromList [0, 0], fromList [0])]
  let n' = trainNTimes n t dat 1000
  
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

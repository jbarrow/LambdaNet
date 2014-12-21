import Network.Neuron
import Network.Layer
import Network.Network
import Network.Trainer
import System.IO
import System.Random
import Numeric.LinearAlgebra

main = do
  let l = LayerDefinition sigmoidNeuron 2 connectFully
  let l' = LayerDefinition sigmoidNeuron 2 connectFully
  let l'' = LayerDefinition sigmoidNeuron 1 connectFully
  let g = mkStdGen 4
  let n = createNetwork normals g [l, l', l'']
  let t = BackpropTrainer 0.3 quadraticCost quadraticCost'
  print $ predict (fromList [0.0, 1.0]) n
  let n' = fit t n [(fromList [0.0, 1.0], fromList [1.0]), (fromList [1.0, 1.0], fromList [0.0]), (fromList [1.0, 0.0], fromList [1.0]), (fromList [0.0, 0.0], fromList [0.0])]
  print $ (weightMatrix (head $ layers n'))
  print $ (weightMatrix (last $ layers n'))
  print $ predict (fromList [0.0, 1.0]) n'

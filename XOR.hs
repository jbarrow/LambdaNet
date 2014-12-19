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
  print $ inputs (fromList [1.0, 1.0]) n

{-# LANGUAGE FlexibleContexts #-}

import AI.Neuron
import AI.Layer

import AI.Network
import AI.Network.SOM

import System.Random

main :: IO ()
main = do
  let l = MapDefinition 2 2 3

  g <- newStdGen

  let n = createNetwork normals g l
  print $ neuronMap n

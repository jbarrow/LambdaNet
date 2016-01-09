module Main where

import Test.Hspec

import TestDemoNeuron
import TestNeuron
import TestLayer

main :: IO()
main = do
  testDemoNeuron
  testNeuron
  testLayer

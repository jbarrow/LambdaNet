module Main where

import Test.Hspec

import TestNeuron
import TestLayer

main :: IO()
main = do
  testNeuron
  testLayer

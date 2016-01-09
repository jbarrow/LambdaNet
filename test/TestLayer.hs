module TestLayer where

import Test.Hspec
import Test.QuickCheck

import AI.Neuron
import AI.Layer

testLayer :: IO ()
testLayer = hspec $
  describe "Layer" $
    it "should be able to pass tests" $
      1 `shouldBe` 1

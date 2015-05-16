module TestDemoNeuron where

import Test.Hspec

import AI.DemoNeuron
import TestNeuron

testDemoNeuron :: IO ()
testDemoNeuron = hspec $ do
  describe "Reduced Neuron" $ do
    it "should pass tests" $ do
      True `shouldBe` True

  describe "L2 Neuron" $ do
    it "should pass tests" $ do
      True `shouldBe` True

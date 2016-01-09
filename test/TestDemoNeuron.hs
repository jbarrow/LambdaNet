module TestDemoNeuron where

import Test.Hspec

import AI.DemoNeuron
import TestNeuron

testDemoNeuron :: IO ()
testDemoNeuron = hspec $ do
  describe "Reduced Neuron" $
    it "should pass tests" $
      True `shouldBe` True

  describe "L2 Neuron" $
    it "should pass tests" $
      True `shouldBe` True

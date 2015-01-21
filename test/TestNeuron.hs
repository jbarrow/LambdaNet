module TestNeuron where

import Test.Hspec
import Test.QuickCheck

import Network.Neuron
import qualified Network.DemoNeuron as D

testNeuron :: IO ()
testNeuron = hspec $ do
  describe "Sigmoid Neuron" $ do
   it "should have a proper description" $ do
      description sigmoidNeuron `shouldBe` "sigmoid"
    
testDemoNeuron :: IO ()
testDemoNeuron = hspec $ do
  describe "Demo Sigmoid Neuron" $ do
    it "should have a proper description" $ do
      show D.SigmoidNeuron `shouldBe` "Sigmoid Neuron"

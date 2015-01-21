module TestNeuron where

import Test.Hspec
import Test.QuickCheck

import Network.Neuron

testNeuron :: IO ()
testNeuron = hspec $ do
  describe "test sigmoid neuron" $ do
    it "should have a proper description" $ do
      description sigmoidNeuron `shouldBe` "sigmoid"
    

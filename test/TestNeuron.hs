module TestNeuron where

import Test.Hspec
import Test.QuickCheck

import Network.Neuron

testNeuron :: IO ()
testNeuron = hspec $ do
  describe "Sigmoid Neuron" $ do
   it "should have a proper description" $ do
      description sigmoidNeuron `shouldBe` "sigmoid"
    

module TestNeuron where

import Test.Hspec
import Test.QuickCheck

import qualified Network.DemoNeuron
    
testDemoNeuron :: IO ()
testDemoNeuron = hspec $ do
  describe "Demo Sigmoid Neuron" $ do
    it "should have a proper description" $ do
      show SigmoidNeuron `shouldBe` "Sigmoid Neuron"
  

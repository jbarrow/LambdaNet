module TestLayer where

import Test.Hspec
import Test.QuickCheck

import Network.Neuron
import Network.Layer

testDemoLayer :: IO ()
testDemoLayer = hspec $ do
  describe "Demo Layer" $ do
    it "should be able to pass tests" $ do
      1 `shouldBe` 1

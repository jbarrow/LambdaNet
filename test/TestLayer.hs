module TestLayer where

import Test.Hspec
import Test.QuickCheck

import AI.DemoNeuron
import AI.DemoLayer

testLayer :: IO ()
testLayer = hspec $ do
  describe "Layer" $ do
    it "should be able to pass tests" $ do
      1 `shouldBe` 1

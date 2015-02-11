module TestLayer where

import Test.Hspec
import Test.QuickCheck

import AI.Neuron
import AI.Layer

testLayer :: IO ()
testLayer = hspec $ do
  describe "Demo Layer" $ do
    it "should be able to pass tests" $ do
      1 `shouldBe` 1

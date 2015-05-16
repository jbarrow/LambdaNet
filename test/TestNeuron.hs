module TestNeuron where

import Test.Hspec
import Data.Typeable

import AI.Neuron

-- Below are helper functions that are used among multiple tests
-- for activation types. They are intended to roughly verify properties
-- of activation functions.


-- | Test that an activation function has a lower bound within a given
--   epsilon.
testLowerBound :: ActivationFunction -> Double -> Bool
testLowerBound f lower = and $ map (>= lower) applied
    where applied = map f [-100.0, -100.1 .. -130.0]

testApproachesLowerBound :: ActivationFunction -> Double -> Double -> Bool
testApproachesLowerBound  f lower epsilon = and $ map (<= lower + epsilon) applied
    where applied = map f [-100.0, -100.1 .. -130.0]
                    
-- | Test that an activation function has a specific upper bound within
--   a given espilon.
testUpperBound :: ActivationFunction -> Double -> Bool
testUpperBound f upper = and $ map (<= upper) applied
    where applied = map f [100.0, 100.1 .. 130.0]

testApproachesUpperBound :: ActivationFunction -> Double -> Double -> Bool
testApproachesUpperBound  f upper epsilon = and $ map (>= upper - epsilon) applied
    where applied = map f [100.0, 100.1 .. 130.0]

-- | Test that an activation function is monotonically increasing (for now,
--   no formal proof, just a band around 0).
testMonotonicallyIncreasing :: ActivationFunction -> Bool
testMonotonicallyIncreasing f = and $ zipWith (<=) applied (drop 1 applied)
    where applied = map f [-10.0, -9.99 .. 10.0]

-- | Test the maximum of a function -- left to do for later
testMaximum :: ActivationFunction -> Double -> Bool
testMaximum f center = (maximum $ map f lst) == (f center)
    where lst = [-10.0 - center, -9.99 - center .. 10.0 + center]
                    
testNeuron :: IO ()
testNeuron = hspec $ do
  describe "Sigmoid Neuron" $ do
    it "should be an instance of Show" $ do
      (typeOf $ show sigmoidNeuron) `shouldBe` typeRep ["Char"]
                       
    it "should have a monotonically increasing activation function" $ do
       (testMonotonicallyIncreasing sigmoid) `shouldBe` True

    it "should have an upper bound at one" $ do
        (testUpperBound sigmoid 1) `shouldBe` True

    it "should have a lower bound at zero" $ do
        (testLowerBound sigmoid 0) `shouldBe` True

    it "should approach the zero lower bound" $ do
        (testApproachesLowerBound sigmoid 0 0.000000001) `shouldBe` True

    it "should approach the one upper bound" $ do
        (testApproachesUpperBound sigmoid 1 0.0000000001) `shouldBe` True

    it "should have a derivative with a maximum at 0" $ do
        (testMaximum sigmoid' 0) `shouldBe` True

  describe "Tanh Neuron" $ do
    it "should be an instance of Show" $ do
      (typeOf $ show tanhNeuron) `shouldBe` typeRep ["Char"]
                       
    it "should have a monotonically increasing activation function" $ do
       (testMonotonicallyIncreasing tanh) `shouldBe` True

    it "should have an upper bound at one" $ do
        (testUpperBound tanh 1) `shouldBe` True

    it "should have a lower bound at negative one" $ do
        (testLowerBound tanh (-1)) `shouldBe` True

    it "should approach the zero lower bound" $ do
        (testApproachesLowerBound tanh (-1) 0.000000001) `shouldBe` True

    it "should approach the one upper bound" $ do
        (testApproachesUpperBound tanh 1 0.0000000001) `shouldBe` True

    it "should have a derivative with a maximum at 0" $ do
        (testMaximum tanh' 0) `shouldBe` True
                                                       
  describe "Reclu Neuron" $ do
    it "should be an instance of Show" $ do
      (typeOf $ show recluNeuron) `shouldBe` typeRep ["Char"]
                       
    it "should have a monotonically increasing activation function" $ do
       (testMonotonicallyIncreasing reclu) `shouldBe` True

    it "should have a lower bound at negative zero" $ do
        (testLowerBound reclu 0) `shouldBe` True

    it "should approach the zero lower bound" $ do
        (testApproachesLowerBound reclu 0 0.000000001) `shouldBe` True

    it "should have a derivative that is monotonically increasing" $ do
       (testMonotonicallyIncreasing reclu') `shouldBe` True

    it "should have a derivative with an upper bound at one" $ do
        (testUpperBound reclu' 1) `shouldBe` True

    it "should have a derivative with a lower bound at zero" $ do
        (testLowerBound reclu' 0) `shouldBe` True

    it "should a derivative that approaches the zero lower bound" $ do
        (testApproachesLowerBound reclu' 0 0.000000001) `shouldBe` True

    it "should a derivative that approaches the one upper bound" $ do
        (testApproachesUpperBound reclu' 1 0.0000000001) `shouldBe` True

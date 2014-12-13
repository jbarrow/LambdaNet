import Network.Neuron
import Network.Layer
import Network.Trainer
import Network.Network
import System.IO
import Text.Printf
import Control.Monad
import Linear

-- Compute a numerical approximation of a function on the range from
-- -10 to 10 with step sizes of 0.01
computeApproximation :: (Float -> Float) -> [Float]
computeApproximation f = map f lst
                          where lst = [-10, -9.99 .. 10] :: [Float]

-- Write a float list to a given file name with a given precision
writeDat filename lst prec =
  withFile filename WriteMode $ \h ->
    let writeLine = hPrintf h $ "%." ++ show prec ++ "g\n" in
      mapM_ writeLine lst

-- Usage:
--  > writeDat "docs/sigmoid.txt" (computeApproximation sigmoid) 5
--  > writeDat "docs/reclu.txt" (computeApproximation reclu) 5
--  > writeDat "docs/tanh.txt" (computeApproximation tanh) 5
--  > writeDat "docs/derivative_sigmoid.txt" (computeApproximation sigmoid') 5
--  > writeDat "docs/derivative_reclu.txt" (computeApproximation reclu') 5
--  > writeDat "docs/derivative_tanh.txt" (computeApproximation tanh') 5

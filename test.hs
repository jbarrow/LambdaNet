import Network

computeApproximation :: (Float -> Float) -> [Float]
computeApproximation f = map f lst
                          where lst = [-10, -9.99 .. 10] :: [Float]

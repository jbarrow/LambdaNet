module Linear
( Matrix
, Vector

, dot
, mMult
, hadamard
, reshape
, randomList
, boxMuller
, normals
, uniforms
) where

type Vector a = [a]
type Matrix a = [[a]]

dot :: (Num a) => Vector a -> Vector a -> a
dot u v = sum $ zipWith (*) u v

mMult :: (Num a) => Matrix a -> Vector a -> Vector a
mMult m v = map (dot v) m

hadamard :: (Num a) => Matrix a -> Matrix a -> Matrix a
hadamard m n = zipWith (zipWith (*)) m n

-- parameters
--   j is the number of columns of the resulting matrix
-- returns
--   i by j matrix where i is length of list / j
reshape :: (Num a) => Int -> [a] -> Matrix a
reshape j [] = []
reshape j list = [(take j list)] ++ reshape j (drop j list)

-- Initialize an infinite random list list with:
randomList :: (RandomGen g, Random a, Floating a) => RandomTransform a -> g -> [a]
randomList transform = transform . randoms

-- Define a transformation on the uniform distribution to generate
-- normally distributed numbers in Haskell (the Box-Muller transform)
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller x1 x2 = (z1, z2) where z1 = sqrt ((-2) * log x1) * cos (2 * pi * x2)
                                 z2 = sqrt ((-2) * log x1) * sin (2 * pi * x2)

-- Apply the Box-Muller transform
normals :: Floating a => [a] -> [a]
normals (x1:x2:xs) = z1:z2:(normals xs) where (z1, z2) = boxMuller x1 x2
normals _ = []

uniforms :: Floating a => [a] -> [a]
uniforms xs = xs

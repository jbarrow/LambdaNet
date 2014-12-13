module Linear
( Matrix
, Vector

, dot
, mMult
, hadamard
, reshape
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

module Linear
( Matrix
, Vector

, mulVV
, mulMV
) where

type Vector a = [a]
type Matrix a = [[a]]

mulVV :: (Num a) => Vector a -> Vector a -> a
mulVV u v = sum $ zipWith (*) u v

mulMV :: (Num a) => Matrix a -> Vector a -> Vector a
mulMV m v = map (mulVV v) m

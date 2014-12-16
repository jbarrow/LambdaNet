module Linear
( Matrix
, RandomTransform

, rows
, cols
, dot
, add
, sub
, mult
, hadamard
, transpose
, scalar
, combine
, reshape
, randomList
, boxMuller
, normals
, uniforms
) where

import System.Random
import Data.List (permutations)

type Matrix a = [[a]]

rows :: (Num a) => Matrix a -> Int
rows = length

cols :: (Num a) => Matrix a -> Int
cols = length . head

-- need to modify this for proper error handling with Maybe
dot :: (Num a) => Matrix a -> Matrix a -> a
dot [u] [v] = sum $ zipWith (*) u v
dot u v = 0

-- matrix addition
add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add m n = zipWith (zipWith (+)) m n

-- matrix subtraction
sub :: (Num a) => Matrix a -> Matrix a -> Matrix a
sub m n = zipWith (zipWith (-)) m n

-- matrix multiplication m * n
mult :: (Num a) => Matrix a -> Matrix a -> Matrix a
mult [] n = []
mult m n = map (dot [head m]) [[x] | x <- (transpose n)] : mult (drop 1 m) n

hadamard :: (Num a) => Matrix a -> Matrix a -> Matrix a
hadamard m n = zipWith (zipWith (*)) m n

-- Lovingly ripped from Data.List, type signature adjusted
transpose :: (Num a) => Matrix a -> Matrix a
transpose [] = []
transpose ([]: xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

scalar :: (Num a) => Matrix a -> a -> Matrix a
scalar mat n = map (map (* n)) mat

combine:: (Num a) => Matrix a -> Matrix a -> Matrix a
combine m n
    | cols m == cols n = m ++ n
    | rows m == rows n = zipWith (++) m n
    | otherwise = m

-- parameters
--   j is the number of columns of the resulting matrix
-- returns
--   i by j matrix where i is length of list / j
reshape :: (Num a) => Int -> [a] -> Matrix a
reshape j [] = []
reshape j list = [(take j list)] ++ reshape j (drop j list)

-- Random Transformations

type RandomTransform a = [a] -> [a]

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

-- Random shuffle
-- grabs random permutation of list
-- order is strictly and consistently mapped
-- providing the same random number yields the same permutation
-- randomShuffle :: [a] -> a -> [a]
-- randomShuffle n r = permutations n !! r

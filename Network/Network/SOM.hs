module Network.Network.SOM
( SOM(..)

) where

data SOM = SOM { map :: Matrix (Vector Double) }

instance Network (SOM) where
  predict inputs network = inputs
  createNetwork transformation g defs = SOM $ fromLists [[fromList [1.0] :: Vector Double]]

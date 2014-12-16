{-# LANGUAGE DeriveGeneric #-}
module Parse
( LayerParseDefinition(..)
, NetworkParseDefinition(..)
, TrainingParseDefinition(..)
, InputParseDefinition(..)
                        ) where
import Network.Network
import Data.Aeson
import GHC.Generics
import Linear

data LayerParseDefinition = LayerParseDefinition { ntype :: String 
                                         , ncount :: Int
                                         , connectivity :: String
                                         , id :: String
                                         } deriving (Generic, Show)

instance FromJSON LayerParseDefinition
instance ToJSON LayerParseDefinition

-- to extend for other types of neurons, pattern match on neuron
-- TODO: change result of otherwise to something more error-like
toLayerDefinition :: LayerParseDefinition -> LayerDefinition a
toLayerDefinition d {ntype=neuron, ncount=count, connectivity=conn}
    | neuron == "sigmoidNeuron" = LayerDefinition {neuronDef=sigmoidNeuron, neuronCount=count, connect=conn}
    | otherwise = LayerDefinition {neuronDef=sigmoidNeuron, neuronCount=count, connect=conn}

data NetworkParseDefinition = NetworkParseDefinition { layers :: [LayerParseDefinition]
                                                     , init :: String
                                         } deriving (Generic, Show)

instance FromJSON NetworkParseDefinition
instance ToJSON NetworkParseDefinition

--toNetworkDefinition :: NetworkParseDefinition -> Network a
--toNetworkDefinition d {layers=layerDefs, init=initDistribution}
--    | init == "normal" = createNetwork  
--    | init == "uniform" = 
--    | otherwise = 

data TrainingParseDefinition = TrainingParseDefinition { trainingdata :: [(Matrix Float, Matrix Float)]
                                                       , nw :: [Matrix Float]
                                         } deriving (Generic, Show)

instance FromJSON TrainingParseDefinition
instance ToJSON TrainingParseDefinition

data InputParseDefinition = InputParseDefinition { inputs :: [Float]
                                                 , network :: [Matrix Float]
                                         } deriving (Generic, Show)

instance FromJSON InputParseDefinition
instance ToJSON InputParseDefinition

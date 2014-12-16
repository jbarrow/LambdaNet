{-# LANGUAGE DeriveGeneric #-}
module Parse
( LayerParseDefinition(..)
, NetworkParseDefinition(..)
, TrainingParseDefinition(..)
, InputParseDefinition(..)
, toMatrixFloat
, toNetwork
                        ) where
import Network.Layer
import Network.Neuron
import Network.Network
import Data.Aeson
import GHC.Generics
import Linear
import System.Random

data LayerParseDefinition = LayerParseDefinition { ntype :: String
                                         , ncount :: Int
                                         , connectivity :: String
                                         , id :: String
                                         } deriving (Generic, Show)

instance FromJSON LayerParseDefinition
instance ToJSON LayerParseDefinition

-- to extend for other types of neurons, pattern match on neuron
-- TODO: change result of otherwise to something more error-like
toLayerDefinition :: (Floating a) => LayerParseDefinition -> LayerDefinition a
toLayerDefinition LayerParseDefinition {ntype=neuron, ncount=count, connectivity=conn}
    | neuron == "sigmoidNeuron" = LayerDefinition {neuronDef=sigmoidNeuron, neuronCount=count, connect=connectFully}
    | otherwise = LayerDefinition {neuronDef=sigmoidNeuron, neuronCount=count, connect=connectFully}

data NetworkParseDefinition = NetworkParseDefinition { layerDefs :: [LayerParseDefinition]
                                                     , initDist :: String
                                         } deriving (Generic, Show)

instance FromJSON NetworkParseDefinition
instance ToJSON NetworkParseDefinition

toNetwork :: (Random a, Floating a) => NetworkParseDefinition -> Network a
toNetwork NetworkParseDefinition {layerDefs=layers, initDist=initDistribution}
    | initDistribution == "normals" = createNetwork normals (mkStdGen 4) (map toLayerDefinition layers)
    | initDistribution == "uniforms" = createNetwork uniforms (mkStdGen 4) (map toLayerDefinition layers)
    | otherwise = createNetwork uniforms (mkStdGen 4) (map toLayerDefinition layers)

data TrainingParseDefinition = TrainingParseDefinition { trainingdata :: [(Matrix Float, Matrix Float)]
                                                       , nw :: [Matrix Float]
                                         } deriving (Generic, Show)

instance FromJSON TrainingParseDefinition
instance ToJSON TrainingParseDefinition

--toTrainingDefinition :: TrainingParseDefinition -> TrainingData a
--toTrainingDefinition TrainingParseDefinition {trainingdata=data,nw=network} = 

data InputParseDefinition = InputParseDefinition { inputs :: [Float]
                                                 , network :: [Matrix Float]
                                         } deriving (Generic, Show)

instance FromJSON InputParseDefinition
instance ToJSON InputParseDefinition


toMatrixFloat :: (Floating a) => Network a -> [Matrix a]
toMatrixFloat n = zipWith combine (map weightMatrix (layers n)) (map biasMatrix (layers n)) 

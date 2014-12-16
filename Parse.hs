{-# LANGUAGE DeriveGeneric #-}
module Parse
( LayerParseDefinition(..)
, NetworkParseDefinition(..)
, TrainingParseDefinition(..)
, InputParseDefinition(..)
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

data NetworkParseDefinition = NetworkParseDefinition { layerDefs :: [LayerParseDefinition]
                                                     , initDist :: String
                                         } deriving (Generic, Show)

instance FromJSON NetworkParseDefinition
instance ToJSON NetworkParseDefinition

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

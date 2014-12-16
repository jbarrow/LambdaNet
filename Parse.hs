{-# LANGUAGE DeriveGeneric #-}
module Parse
( LayerParseDefinition(..)
, NetworkParseDefinition(..)
                        ) where
import Network.Layer
import Data.Aeson
import GHC.Generics

data LayerParseDefinition = LayerParseDefinition { neuronType :: String 
                                         , count :: Int
                                         , connect :: String
                                         } deriving (Generic, Show)

instance FromJSON LayerParseDefinition
instance ToJSON LayerParseDefinition

data NetworkParseDefinition = NetworkParseDefinition { layers :: [LayerParseDefinition]
                                                     , init :: String
                                         } deriving (Generic, Show)

instance FromJSON NetworkParseDefinition
instance ToJSON NetworkParseDefinition

{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Main where

import Web.Scotty

import Control.Applicative ((<$>), (<*>), empty)
        
import Data.Default (def)
import Data.Aeson
import Data.Text.Lazy.Encoding
        

-- Network
import Network.Wai.Handler.Warp (settingsPort)
import Network.HTTP.Types  (status404, status200)
import Network.Layer
import Parse

------------------------------------------------------------------------------

config :: Options
config = def { verbose = 0
           , settings = (settings def) { settingsPort = 4000 } }

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)

instance FromJSON Coord where
    parseJSON (Object v) = Coord <$>
                           v .: "x" <*>
                             v .: "y"
    parseJSON _          = empty

------------------------------------------------------------------------------

mirror = do
  wb <- body
  text $ decodeUtf8 wb

main :: IO ()
main = do
    putStrLn "Starting HTTP Server."
    -- let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
    let ld = decode "{\"count\":3,\"connect\":\"fully\",\"neuronType\":\"sigmoid\"}" :: Maybe LayerParseDefinition
    print ld
    let nd = decode "{\"layers\": [{\"count\":3,\"connect\":\"fully\",\"neuronType\":\"sigmoid\"}], \"init\": \"normal\"" :: Maybe NetworkParseDefinition
    print nd
    scottyOpts config $ do
        -- /create {layers: [LayerDefinition], init: String}
        post "/create/" $ mirror
        -- /train {data: training data (not sure what form), network: Network}
        post "/train/" $ mirror
        -- /eval {inputs: [Int?], network: Network}
        post "/eval" $ mirror


------------------------------------------------------------------------------

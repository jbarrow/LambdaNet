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
import Linear
import Parse

------------------------------------------------------------------------------

config :: Options
config = def { verbose = 0
           , settings = (settings def) { settingsPort = 4000 } }

------------------------------------------------------------------------------

mirror = do
  wb <- body
  text $ decodeUtf8 wb

create = do
  wb <- body
  let d  = decode wb :: Maybe NetworkParseDefinition 
  -- take d, a NetworkParseDefinition, and turn it into a Network Definition, then run it through createNetwork.
  -- finally, return `text $ decodeUtf8 $ encode newly_created_network`.
  text $ decodeUtf8 $ encode d

train = do
  wb <- body
  let d  = decode wb :: Maybe TrainingParseDefinition 
  -- take d, a TrainingParseDefinition, turn it TrainingData type + Network Type, then run them through train.
  -- finally, return `text $ decodeUtf8 $ encode trained_network`.
  text $ decodeUtf8 $ encode $ case d of
                                  Just value -> nw value :: [Matrix Float]
                                  Nothing    -> []

eval = do
  wb <- body
  let d  = decode wb :: Maybe InputParseDefinition 
  -- take d and run the input through the provided network. Finally, return `text $ decodeUtf8 $ encode result`.
  text $ decodeUtf8 $ encode $ case d of
                                  Just value -> inputs value :: [Float]
                                  Nothing    -> []

main :: IO ()
main = do
    putStrLn "Starting HTTP Server."
    -- let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
    let ld = decode "{\"ncount\":3,\"connectivity\":\"fully-connected\",\"ntype\":\"sigmoid\",\"id\":\"tnser\"}" :: Maybe LayerParseDefinition
    print ld
    let nd = decode "{\"layers\": [{\"id\": \"abc\",\"ncount\":3,\"connectivity\":\"fully-connected\",\"ntype\":\"sigmoid\"}], \"init\": \"normal\"}" :: Maybe NetworkParseDefinition
    print nd
    let td = decode "{\"trainingdata\": [[[[1.0, 0.0]], [[1.0]]], [[[1.0, 1.0]], [[0.0]]], [[[0.0, 1.0]], [[1.0]]], [[[0.0, 0.0]], [[0.0]]]], \"nw\":[[[0.5,0,1],[0.5,1,0]],[[1,1,1]]]}" :: Maybe TrainingParseDefinition
    print td
    let id = decode "{\"inputs\": [1.0, 0.0], \"network\":[[[0.5,0,1],[0.5,1,0]],[[1,1,1]]]}" :: Maybe InputParseDefinition
    print id
    scottyOpts config $ do
        -- /create {layers: [LayerDefinition], init: String}
        post "/create/" create

        -- /train {data: training data (not sure what form), network: Network}
        post "/train/" train

        -- /eval {inputs: [Int?], network: Network}
        post "/eval/" eval


------------------------------------------------------------------------------






{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Main where

import Web.Scotty

import Control.Monad.IO.Class
import Control.Applicative ((<$>), (<*>), empty)
        
import Data.Default (def)
import Data.Typeable
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
        

-- Network
import Network.Wai.Handler.Warp (settingsPort)
import Network.HTTP.Types  (status404, status200)

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
  text $ decodeASCII wb

main :: IO ()
main = do
    putStrLn "Starting HTTP Server."
    let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
    print req
    scottyOpts config $ do
        -- /create {layers: [LayerDefinition], init: String}
        post "/create/" $ mirror
        -- /train {data: training data (not sure what form), network: Network}
        post "/train/" $ mirror
        -- /eval {inputs: [Int?], network: Network}
        post "/eval" $ mirror


------------------------------------------------------------------------------

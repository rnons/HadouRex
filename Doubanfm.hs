{-# LANGUAGE DeriveDataTypeable #-}
module Doubanfm where
{-( play
, getPlaylist
, Config
) where-}

import Network.HTTP
import Text.JSON
import Text.JSON.Generic
import System.Cmd
import GHC.IO.Exception
import Codec.Binary.UTF8.String
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Global.Config

data Config = Config { cid :: Int } deriving (Show, Typeable)

instance Default Config where
    def = Config 0 

instance GlobalConfig Config where
    onSetConfig = liftIO . print

-- there are two types of json response: song & ad.
-- the 'company', 'rating_avg', 'public_time', 'ssid' fields are missing in ad_json
-- the 'like' and 'length' fields are of String type in ad_json
data Song = Song {
    picture :: String,
    albumtitle :: String,
    --company :: String,
    --rating_avg :: Float,
    --public_time :: String,
    --ssid :: String,
    album :: String,
    --like :: Int,
    artist :: String,
    url :: String,
    title :: String,
    subtype :: String,
    --length :: Int,
    sid :: String,
    aid :: String
} deriving (Eq, Show, Data, Typeable)

data Playlist = Playlist {
    r :: Int,
    song :: [Song] 
} deriving (Eq, Show, Data, Typeable)

getPlaylist = do
    conf <- getConfig
    let channel_id = cid conf
    let url = "http://douban.fm/j/mine/playlist?channel=" ++ show channel_id ++ "&type=n"
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    -- Chinese characters needed to be decoded by decodeString
    let playlist = decodeJSON $ decodeString json :: Playlist
    play $ song playlist

play :: [Song] -> IO GHC.IO.Exception.ExitCode
play [] = getPlaylist
play (x:xs) = do
    putStrLn $ artist x ++ " - " ++ title x
    rawSystem "mpg123" ["-C", url x]
    play xs

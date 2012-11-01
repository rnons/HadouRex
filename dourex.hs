{-# LANGUAGE DeriveDataTypeable #-}

import Network.HTTP
import Text.JSON
import Text.JSON.Generic
import System.Cmd
import GHC.IO.Exception

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

main = do
    rsp <- Network.HTTP.simpleHTTP (getRequest "http://douban.fm/j/mine/playlist?channel=6&type=n")
    json <- getResponseBody rsp
    let parsed = decodeJSON json :: Playlist
    play $ song parsed
    --let song_url = url $ head $ song parsed
    --rawSystem "mpg123" ["-C", song_url]

play :: [Song] -> IO GHC.IO.Exception.ExitCode
play [] = return ExitSuccess
play (x:xs) = do
    rawSystem "mpg123" ["-C", url x]
    play(xs)

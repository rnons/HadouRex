{-# LANGUAGE DeriveDataTypeable #-}
module Douban.FM  
    (
      Song(..)
    , Channel(..)
    , getPlaylist
    , hot
    , trending
    , search
    , channelInfo
    , musicianID  
    , parseSong
    , parseChannel
    ) where

import Prelude hiding (id)
import Control.Monad
import Data.Char

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Network.HTTP
import Text.HTML.TagSoup
import Text.JSON
import Text.JSON.Generic


-- | There are two types of json response: song & ad.
-- No 'company', 'rating_avg', 'public_time', 'ssid' fields in ad_json.
-- The 'like' and 'length' fields are of String type in ad_json.
data Song = Song {
    picture :: String,
    albumtitle :: String,
    --company :: String,
    --rating_avg :: Float,
    --public_time :: String,
    ssid :: String,
    album :: String,
    like :: Int,
    artist :: String,
    url :: String,
    title :: String,
    subtype :: String,
    --length :: Int,
    sid :: String,
    aid :: String
} deriving (Eq, Show, Data, Typeable)


-- | Channel json response structure.
data Channel = Channel {
    intro :: String,
    name :: String,
    song_num :: Int,
    --creator :: Creator,
    banner :: String,
    cover :: String,
    id :: Int,
    hot_songs :: [String]
} deriving (Eq, Show, Data, Typeable)


-- | If success, a list of song will be returned.
-- Failure not accounted at present.
getPlaylist :: [(String, String)] -> IO [Song]
getPlaylist reqData = do
    let url = "http://douban.fm/j/mine/playlist?" ++ urlEncodeVars reqData
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    return $ parseSong json


-- | Return a list of hot channels.
hot :: IO [Channel]
hot = search' url
  where
    url = "http://douban.fm/j/explore/hot_channels"


-- | Return a list of up trending channels.
trending :: IO [Channel]
trending = search' url
  where
    url = "http://douban.fm/j/explore/up_trending_channels"


-- | Return a list of channels matching provided keywords.
search :: String -> IO [Channel]
search [] = return []
search key = search' url
  where 
    url = "http://douban.fm/j/explore/search?query=" ++ 
          -- encodeString: encode chinese characters
          urlEncode (encodeString key)


search' :: String -> IO [Channel]
search' url = do
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    return $ parseChannel json


-- | Return the channel matching provided channel id.
channelInfo :: String -> IO (Maybe Channel)
channelInfo "0" = return $ Just ch
  where
    ch = Channel { intro = "私人", name = "私人", 
                   song_num = 1001, banner = "", 
                   cover = "", id = 0, hot_songs = [] }
channelInfo cid = do
    let url = "http://douban.fm/j/explore/search?query=" ++ cid
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    let chs = parseChannel json
        match = filter (\c -> id c == read cid) chs
    case match of 
         [] -> return Nothing
         cs -> return $ Just $ head cs


-- | Return musician id matching provided musician name.
musicianID :: String -> IO (Maybe String)
musicianID name = do
    let url = "http://music.douban.com/search/" ++ urlEncode (encodeString name)
    rsp <- simpleHTTP $ getRequest url
    body <- getResponseBody rsp
    let tags = parseTags body
        -- how to use `sections` when there are two class names?
        sec = sections (~== "<div class=answer_left>") tags
    case sec of
         [] -> return Nothing
         _  -> do
             let musician_tags = (head sec) !! 4
                 musician_info = (\(TagOpen "a" x) -> x) musician_tags
                 (_, resp_name) =  musician_info !! 2
                 musician_name = decodeString resp_name
                 (_, href) = musician_info !! 1
             return $ Just $ filter isDigit href


-- | Parse json response of getPlaylist to [Song].
-- Capable of filtering out ads :-)
parseSong :: String -> [Song]
parseSong json = do
    let decoded = decode json :: Result (JSObject JSValue)
    let value = decoded >>= valFromObj "song" :: Result [JSObject JSValue]
    let songs = (\(Ok x) -> x) value
    let filterSong s = do
        let ssid = valFromObj "ssid" s :: Result JSValue
        case ssid of
             Ok _ -> True
             _  -> False
    let ss = filter filterSong songs
    decodeJSON $ decodeString $ encode ss :: [Song]


-- | Parse json response of hot, trending, search to [Channel].
parseChannel :: String -> [Channel]
parseChannel json = do
    let decoded = decode json :: Result (JSObject JSValue)
    let value = decoded >>= valFromObj "data" >>= 
                valFromObj "channels" :: Result [JSObject JSValue]
    let channels = (\(Ok x) -> x) value
    decodeJSON $ decodeString $ encode channels :: [Channel]




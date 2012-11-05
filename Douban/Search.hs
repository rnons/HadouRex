{-# LANGUAGE DeriveDataTypeable #-}
module Douban.Search  where

import Network.HTTP
import Text.JSON
import Text.JSON.Generic
import GHC.IO.Exception
import Codec.Binary.UTF8.String

data Creator = Creator {
    url :: String,
    -- !name conflicts!
    cname :: String
} deriving (Eq, Show, Data, Typeable)

data Channel = Channel {
    intro :: String,
    name :: String,
    song_num :: Int,
    creator :: Creator,
    banner :: String,
    cover :: String,
    id :: Int,
    hot_songs :: [String]
} deriving (Eq, Show, Data, Typeable)

data Channel_list = Channel_list {
    status :: Bool,
    -- !!! the response contains 'data' field !!!
    -- seems like implementing self-made JSON parse is a must
    cdata   :: [Channel]
} deriving (Eq, Show, Data, Typeable)

search [] = do
    putStrLn "Please supply your keywords"
    return GHC.IO.Exception.ExitSuccess
search (key:xs) = do
    putStrLn key
    let url = "http://douban.fm/j/explore/search?query=" ++ key
    putStrLn url
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    -- Chinese characters needed to be decoded by decodeString
    let clist = decodeJSON $ decodeString json :: Channel_list
    print(clist)
    return GHC.IO.Exception.ExitSuccess


{-# LANGUAGE DeriveDataTypeable #-}
module Douban.Search  where

import Prelude hiding (id)
import Network.HTTP
import Text.JSON (decode, encode)
import Text.JSON.Generic
import GHC.IO.Exception
import Codec.Binary.UTF8.String hiding (decode, encode)
import Control.Monad

data Creator = Creator {
    url :: String,
    -- !name conflicts!
    cname :: String
} deriving (Eq, Show, Data, Typeable)

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
    -- encodeString: encode chinese characters
    let url = "http://douban.fm/j/explore/search?query=" ++ urlEncode (encodeString key)
    rsp <- simpleHTTP $ getRequest $ url
    json <- getResponseBody rsp
    let chs = parseChannel json
    forM chs (\c -> do
        putStrLn $ "* " ++ name c
        putStrLn $ "    Channel_id: " ++ show (id c)
        putStrLn $ "    Intro: " ++ intro c
        putStr $ "    Hot songs: " 
        forM (hot_songs c) (\s -> putStr $ s ++ ", ")
        putStrLn ""
        )
    return GHC.IO.Exception.ExitSuccess

parseChannel json = do
    let decoded = decode json :: Result (JSObject JSValue)
    let value = decoded >>= (valFromObj "data") >>= (valFromObj "channels") :: Result [JSObject JSValue]
    -- channels :: [JSOBject JSValue]
    let channels = (\(Ok x) -> x) value
    decodeJSON $ decodeString $ encode channels :: [Channel]



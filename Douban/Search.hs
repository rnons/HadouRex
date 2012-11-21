{-# LANGUAGE DeriveDataTypeable #-}
module Douban.Search  where

import Prelude hiding (id)
import Network.HTTP
import Text.JSON (decode, encode)
import Text.JSON.Generic
import GHC.IO.Exception
import Codec.Binary.UTF8.String hiding (decode, encode)
import Control.Monad
import System.Console.ANSI
import Douban.Utils

data Creator = Creator {
    curl :: String,
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

search [] = do
    putStrLn "Please supply your keywords"
    return ()
search (key:xs) = do
    -- encodeString: encode chinese characters
    let url = "http://douban.fm/j/explore/search?query=" ++ urlEncode (encodeString key)
    searchHelper url

hot _ = do
    let url = "http://douban.fm/j/explore/hot_channels"
    searchHelper url

trending _ = do
    let url = "http://douban.fm/j/explore/up_trending_channels"
    searchHelper url

searchHelper url = do
    rsp <- simpleHTTP $ getRequest url
    json <- getResponseBody rsp
    let chs = parseChannel json
    forM_ chs (\c -> do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr $ "* " ++ name c 
        setSGR [SetColor Foreground Vivid Green]
        putStrLn $ " cid=" ++ show (id c)
        setSGR [Reset]
        --putStrLn $ "    Intro: " ++ folding (intro c) 
        --    where folding = foldr (\x acc -> if x `elem` ['\r', '\n'] then ' ':acc else x:acc) []
        let folding = foldr (\x acc -> if x `elem` "\r\n" then ' ':acc else x:acc) []
        putStrLn $ "    Intro: " ++ folding (intro c) 
        putStr "    Hot songs: " 
        forM_ (hot_songs c) (\s -> putStr $ s ++ ", ")
        putStrLn ""
        )
    shutdown
    --return ()

parseChannel json = do
    let decoded = decode json :: Result (JSObject JSValue)
    let value = decoded >>= valFromObj "data" >>= valFromObj "channels" :: Result [JSObject JSValue]
    -- channels :: [JSOBject JSValue]
    let channels = (\(Ok x) -> x) value
    decodeJSON $ decodeString $ encode channels :: [Channel]



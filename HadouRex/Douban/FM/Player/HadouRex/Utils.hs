{-# LANGUAGE DeriveDataTypeable #-}
module Douban.FM.Player.HadouRex.Utils where

import Prelude hiding (id)
import Control.Monad
import Control.Concurrent
import Data.List
import System.Exit
import System.IO

import System.Directory
import System.Posix.Process     (exitImmediately)
import System.Console.ANSI
import Text.JSON.Generic

import Douban.FM
import Douban.FM.Player.HadouRex.State


data FavChannel = FavChannel {
     fav_id    ::  String
    ,fav_name  ::  String
} deriving (Eq, Show, Data, Typeable)


pprChannels chs = do
    forM_ chs (\c -> do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr $ "* " ++ name c 
        setSGR [SetColor Foreground Vivid Green]
        putStrLn $ " cid=" ++ show (id c)
        setSGR [Reset]
        let folding = foldr (\x acc -> 
                      if x `elem` "\r\n" then ' ':acc else x:acc) []
        putStrLn $ "    Intro: " ++ folding (intro c) 
        putStr "    Hot songs: " 
        forM_ (hot_songs c) (\s -> putStr $ s ++ ", ")
        putStrLn ""
        )
    shutdown


getBookmarkFile = do
    dir <- getAppUserDataDirectory "dourex"
    let filepath = dir ++ "/bookmarks.json"
    exist <- doesFileExist filepath
    if exist then return filepath
             else do
                 createDirectory dir
                 writeFile filepath ""
                 return filepath


mark ("current":xs) = do
    ch_id <- getsST st_ch_id
    ch_name <- getsST st_ch_name
    mark' ch_id ch_name
mark (ch_id:xs) = do
    c <- channelInfo ch_id
    case c of 
         Nothing -> putStrLn "Wrong Channel ID!"
         Just ch -> do
             let ch_name = name ch
             mark' ch_id ch_name
             shutdown
mark _ = do
    showHelp []
    shutdown


mark' ch_id ch_name = do
    let ch = FavChannel { fav_id = ch_id, fav_name = ch_name }
    bookmarks <- getBookmarkFile
    json <- readFile bookmarks
    case json of
         "" -> do
             let newjson = encodeJSON [ch]
             writeFile bookmarks newjson
             putStrLn "Marked"
         _ -> do
             let fav_ch = decodeJSON json :: [FavChannel]
             if ch `elem` fav_ch then putStrLn "Already marked!"
                    else do
                        let newjson = encodeJSON $ ch:fav_ch
                        writeFile bookmarks newjson
                        putStrLn "Marked"


unmark ("current":xs) = do
    ch_id <- getsST st_ch_id
    ch_name <- getsST st_ch_name
    unmark' ch_id ch_name
unmark (ch_id:xs) = do
    c <- channelInfo ch_id
    case c of 
         Nothing -> putStrLn "Wrong Channel ID!"
         Just ch -> do
             let ch_name = name ch
             unmark' ch_id ch_name
             shutdown
unmark _ = do
    showHelp []
    shutdown


unmark' ch_id ch_name = do
    let ch = FavChannel { fav_id = ch_id, fav_name = ch_name }
    bookmarks <- getBookmarkFile
    json <- readFile bookmarks
    case json of
         "" -> do
             let newjson = encodeJSON ch
             writeFile bookmarks newjson
             putStrLn "Not marked!"
         _ -> do
             let fav_ch = decodeJSON json :: [FavChannel]
             if ch `notElem` fav_ch then putStrLn "Not marked!"
                    else do
                        let newjson = encodeJSON $ delete ch fav_ch
                        writeFile bookmarks newjson
                        putStrLn "Unmarked"


marks _ = do
    pprMarks
    shutdown


pprMarks = do
    bookmarks <- getBookmarkFile
    json <- readFile bookmarks
    case json of
         "" -> do
             putStrLn "You have no channel in your bookmark."
             putStrLn "Give a try to `dourex hot` or `dourex trending`."
         _ -> do
             putStrLn "Bookmarked channels:"
             let fav_ch = decodeJSON json :: [FavChannel]
             forM_ fav_ch (\c -> do
                setSGR [SetConsoleIntensity BoldIntensity]
                putStr $ "* " ++ fav_name c 
                setSGR [SetColor Foreground Vivid Green]
                putStr $ " cid=" ++ show (fav_id c)
                setSGR [Reset]
                putStrLn ""     -- [Reset] won't work without this
                )
    

isMarked ch = do
    bookmarks <- getBookmarkFile
    json <- readFile bookmarks
    case json of
         [] -> return False
         _ -> do
             let fav_ch = decodeJSON json :: [FavChannel]
             return $ ch `elem` fav_ch


showHelp arg = do
    let msg = [
            "Usage: dourex channel_id/artist_name"
            ,"   or: dourex COMMAND"
            ,""
            ,"Commands:"
            ,"listen [cid/artist]       listen to channel_id without login"
            ,"search                    Search matching DJ channels"
            ,"hot                       Show hot channels"
            ,"trending                  Show up trending channels"
            ,"mark cid                  bookmark channel"
            ,"unmark cid                unbookmark channel"
            ,"marks                     list bookmarked channel"
            ,"help                      Help messages"
            ]
    putStrLn $ unlines msg
    shutdown


shutdown = do
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    exitImmediately ExitSuccess
    

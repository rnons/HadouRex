{-# LANGUAGE DeriveDataTypeable #-}
module Douban.FM.Player.HadouRex where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import System.Exit
import System.IO
import System.Process           
import System.Console.ANSI

import Network.HTTP
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Text.JSON.Generic

import Douban.FM as FM
import Douban.FM.Player.HadouRex.Utils
import Douban.FM.Player.HadouRex.State


data RecChannel = RecChannel {
    channel_url :: String,
    channel_title :: String
} deriving (Eq, Show, Data, Typeable)


mpgInit = do
    let sh = "mpg123 -R"
    (Just hin, Just hout, _, hdl) <- 
        createProcess (shell sh){ std_in = CreatePipe, std_out=CreatePipe }
    hPutStrLn hin "SILENCE"
    mhin <- newMVar hin
    mhout <- newMVar hout
    silentlyModifyST $ \st -> st { writeh = mhin, readh = mhout }
    waitForProcess hdl
    hPutStrLn hin "STOP"
    hGetContents hout
    hFlush hin
    return ()


hot _ = FM.hot >>= pprChannels


trending _ = FM.trending >>= pprChannels


search [] = putStrLn "Please supply your keywords"
search (key:xs) = FM.search key >>= pprChannels


listen [] = select
listen (x:xs)
    | isChId x = listenCid x
    | otherwise = listenArtist x
    where isChId = and . fmap isDigit


select = do
    pprMarks
    selectChannel

listenCid ch_id = do
    let reqData = [("type", "n"),
                 ("channel", ch_id),
                 ("from", "HadouRex")
                ]
    silentlyModifyST $ \st -> st {  st_ch_id = ch_id }
    forkIO $ play reqData []
    return ()

listenArtist name = do
    mID <- FM.musicianID name
    case mID of
         Nothing -> do
             putStrLn $ "Sorry, douban.fm knows no musician named " ++ name
             select
         Just musician_id -> do
             let musician_name = name
             putStr "您正在收听的是: " 
             putStrLn $ musician_name ++ " MHz"
             silentlyModifyST $ \st -> 
                st { st_ch_id = "0"
                    ,st_ch_name = musician_name
                    ,st_ch_intro = musician_name
                    ,st_context = "channel:0|musician_id:" ++ musician_id
                   }
             let reqData = [("type", "n"),
                            ("channel", "0"),
                            ("context", "channel:0|musician_id:" ++ musician_id),
                            ("from", "HadouRex")
                           ]
             forkIO $ play reqData []
             return ()


play :: [(String,String)] -> [Song] -> IO ()
play reqData [] = getPlaylist reqData >>= play reqData
play reqData (x:xs) = do
    setSGR [SetConsoleIntensity BoldIntensity,
            SetColor Foreground Vivid Green]
    putStrLn ""
    putStr $ artist x ++ " - " ++ title x
    setSGR [Reset]
    putStrLn ""
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hPutStrLn hin $ "LOAD " ++ url x
        hFlush hin
    silentlyModifyST $ \st -> st { st_s_picture     =   picture x,
                                   st_s_albumtitle  =   albumtitle x,
                                   st_s_album       =   album x,
                                   st_s_artist      =   artist x,
                                   st_s_title       =   title x,
                                   st_s_sid         =   sid x,
                                   st_s_ssid        =   ssid x,
                                   st_s_aid         =   aid x
                                 }
    liftIO $ withST $ \st -> do
        hin <- readMVar (writeh st)
        hout <- readMVar (readh st)
        mpg123wait hin hout
    play reqData xs
  where 
    mpg123wait hin hout = do
        line <- hGetLine hout
        case line of
             "@P 0" -> do
                 --reportEnd
                 return ExitSuccess
             _ -> mpg123wait hin hout


selectChannel = do
    putStrLn "Please enter the channel_id you want to listen to:"
    inpStr <- getLine
    -- default channel: "1001294 PostRock Odyssey"
    let channel_id = if inpStr `elem` ["","-3"] then "1001294" else inpStr
    listen [channel_id]


getKey = do
    k <- getChar
    handleKey k
    getKey


handleKey ' ' = pause
handleKey 'p' = pause
handleKey 'n' = send "STOP"
handleKey 'm' = markCurrent
handleKey 'o' = openAlbumPage
handleKey 'r' = recommendChannel
handleKey 's' = recommendSong
handleKey 'q' = do
    send "STOP"
    shutdown
handleKey 'h' = do
    let msg = [
            " -= terminal control keys =-",
            "[p] or [ ] play/pause",
            "[n]    next track",
            "[m]    mark/unmark current channel",
            "[r]    recommend current channel to douban",
            "[o]    open album/musician page",
            "[s]    share current song to douban",
            "[h]    this help",
            "[q]    quit"
          ]
    putStrLn $ unlines msg
handleKey _ = getKey


send msg = withST $ \st -> do
    hin <- readMVar (writeh st)
    hPutStrLn hin msg
    hFlush hin


pause = do
    send "PAUSE"
    status <- getsST status
    if status then do 
                silentlyModifyST $ \st -> st { status = False }            
                putStrLn "Paused"
              else do 
                silentlyModifyST $ \st -> st { status = True }            
                putStrLn "Playing"


markCurrent = do
    -- bookmarks can be changed outside, while st_ch_marked not modified
    ch_id <- getsST st_ch_id
    ch_name <- getsST st_ch_name
    let ch = FavChannel { fav_id = ch_id, fav_name = ch_name }
    ch_marked <- isMarked ch
    if ch_marked then unmark ["current"] else mark ["current"]


recommendChannel = do
    putStrLn "推荐兆赫到豆瓣"
    ch_id     <- getsST st_ch_id
    ch_name   <- getsST st_ch_name
    image   <- getsST st_ch_picture
    desc    <- getsST st_ch_intro
    let href = "http://douban.fm/?cid=" ++ ch_id
    let action_props = RecChannel {channel_url = href,
                         channel_title = encodeString ch_name ++ "MHz - hadourex"
                         }
    let fdata = [("name", encodeString ch_name ++ "MHz"),
                 ("href", href),
                 ("image", image),
                 ("desc", encodeString desc),
                 ("target_type", "rec"),
                 ("target_action", "0"),
                 ("object_kind", "3072"),
                 ("object_id", ch_id),
                 ("action_props", encodeJSON action_props)
                ]
    recommend fdata


recommendSong = do
    putStrLn "分享歌曲到豆瓣"
    ch_id     <- getsST st_ch_id
    ch_name   <- getsST st_ch_name
    name    <- getsST st_s_title
    image   <- getsST st_s_picture
    st_s_sid    <- getsST st_s_sid
    st_s_ssid   <- getsST st_s_ssid
    let start = st_s_sid ++ "g" ++ st_s_ssid ++ "g0"
    let sdata = [("start", start),("cid", ch_id)]
    let href = "http://douban.fm/?" ++ urlEncodeVars sdata
        desc = encodeString $ "(来自hadourex - " ++ ch_name ++ " MHz)"
    let fdata = [("name", encodeString name),
                 ("href", href),
                 ("image", image),
                 ("text", ""),
                 ("desc", desc),
                 ("target_type", "rec"),
                 ("target_action", "0"),
                 ("object_kind", "3043"),
                 ("object_id", st_s_sid)
                ]
    recommend fdata


recommend fdata = do
    let shuo = "http://shuo.douban.com/!service/share?" ++ urlEncodeVars fdata
    forkIO $ do
        createProcess (proc "xdg-open" [shuo]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        return ()
    return ()
    

openAlbumPage = do
    subject_id <- getsST st_s_album
    -- subject_id can be full url
    let url = if head subject_id == 'h' 
              then subject_id
              else "http://music.douban.com" ++ subject_id
    print url
    forkIO $ do
        createProcess (proc "xdg-open" [url]){ 
            std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        return ()
    return ()

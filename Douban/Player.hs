{-# LANGUAGE DeriveDataTypeable #-}
module Douban.Player where

import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process
import Control.Concurrent
import Douban.FM
import Douban.Utils
import Douban.State
import Network.HTTP
import Codec.Binary.UTF8.String (encodeString)
import Text.JSON.Generic

data RecChannel = RecChannel {
    channel_url :: String,
    channel_title :: String
} deriving (Eq, Show, Data, Typeable)

mpgLoop = do
    let sh = "mpg123 -R"
    (Just hin, Just hout, _, hdl) <- createProcess (shell sh){ std_in = CreatePipe, std_out=CreatePipe }
    hPutStrLn hin "SILENCE"
    mhin <- newMVar hin
    mhout <- newMVar hout
    silentlyModifyST $ \st -> st { writeh = mhin, readh = mhout }
    waitForProcess hdl
    hPutStrLn hin "STOP"
    hGetContents hout
    hFlush hin
    return ()

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
    recommendCore fdata

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
                 ("text", "Testing HadouRex, never mind.."),
                 ("desc", desc),
                 ("target_type", "rec"),
                 ("target_action", "0"),
                 ("object_kind", "3043"),
                 ("object_id", st_s_sid)
                ]
    recommendCore fdata

recommendCore fdata = do
    let shuo = "http://shuo.douban.com/!service/share?" ++ urlEncodeVars fdata
    forkIO $ do
        createProcess (proc "xdg-open" [shuo]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        return ()
    return ()
    
openAlbumPage = do
    subject_id <- getsST st_s_album
    --let url = "http://music.douban.com" ++ subject_id
    let url = if head subject_id == 'h' then subject_id
                                        else "http://music.douban.com" ++ subject_id
    print url
    forkIO $ do
        createProcess (proc "xdg-open" [url]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        return ()
    return ()

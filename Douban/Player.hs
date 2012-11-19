module Douban.Player where

import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process
import Control.Concurrent
import Douban.FM
import Douban.Util
import Douban.State
import Network.HTTP
import Codec.Binary.UTF8.String (encodeString)

mstatus = unsafePerformIO $ newMVar True

mpgLoop = do
    let sh = "mpg123 -R"
    (Just hin, Just hout, _, hdl) <- createProcess (shell sh){ std_in = CreatePipe, std_out=CreatePipe }
    hPutStrLn hin "SILENCE"
    --putMVar mhin hin
    --putMVar mhout hout
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

handleKey ' ' = do
    pause
handleKey 'p' = do
    pause
handleKey 'n' = do
    send "STOP"
handleKey 'm' = do
    markCurrent
handleKey 'r' = do
    recommend
handleKey 'q' = do
    send "STOP"
    shutdown
handleKey 'h' = do
    let msg = [
            " -= terminal control keys =-",
            "[p] or [ ] play/pause",
            "[n]    next track",
            "[m]    mark/unmark current channel",
            "[r]    recommend to douban",
            "[h]    this help",
            "[q]    quit"
          ]
    putStrLn $ unlines msg
    --forM msg (\m -> putStrLn msg)
    {-
    print " -= terminal control keys =-\
            \[p] or [ ] play/pause\
            \[s]    skip track\
            \[h]    this help\
            \[q]    quit\
            \"-}
handleKey _ = getKey

send msg = do
    --hin <- readMVar mhin
    --hin <- readMVar (getsST chin)
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hPutStrLn hin msg
        hFlush hin

pause = do
    send "PAUSE"
    status <- readMVar mstatus
    if status == True then do 
                        swapMVar mstatus False 
                        putStrLn "Paused"
                      else do 
                        swapMVar mstatus True
                        putStrLn "Playing"
    {-
    hout <- readMVar mhout
    line <- hGetLine hout
    hFlush hout
    case line of
         "@P 1" -> putStrLn "Playing"
         "@P 2" -> putStrLn "Paused"
    -}

markCurrent = do
    ch_marked <- getsST st_ch_marked
    if ch_marked == True then unmark ["current"] else mark ["current"]

recommend = do
    putStrLn "Recommend to douban"
    cid     <- getsST ccid
    cname   <- getsST channel_name
    name    <- getsST ctitle
    --href    <- getsST calbum
    image   <- getsST cpicture
    csid    <- getsST csid
    cssid   <- getsST cssid
    let start = csid ++ "g" ++ cssid ++ "g0"
    let sdata = [("start", start),("cid", cid)]
    let href = "http://douban.fm/?" ++ urlEncodeVars sdata
        desc = encodeString $ "(来自dourex-" ++ cname ++ " MHz)"
    let fdata = [("name", encodeString name),
                 ("href", href),
                 ("image", image),
                 ("text", "Testing HadouRex, never mind.."),
                 ("desc", desc),
                 --("apikey", "0d29425036aedcc4277bc6f0a40c964c"),
                 ("target_type", "rec"),
                 ("target_action", "0"),
                 ("object_kind", "3043"),
                 ("object_id", csid)
                ]
    let shuo = "http://shuo.douban.com/!service/share?" ++ urlEncodeVars fdata
    forkIO $ do
        createProcess (proc "xdg-open" [shuo]){ std_in = CreatePipe, std_out=CreatePipe, std_err = CreatePipe }
        --rawSystem "xdg-open" [shuo]
        return ()
    return ()

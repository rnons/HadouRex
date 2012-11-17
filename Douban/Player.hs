module Douban.Player where

import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process
import Control.Concurrent
import Douban.Util

mhin = unsafePerformIO $ newEmptyMVar
mhout = unsafePerformIO $ newEmptyMVar
mstatus = unsafePerformIO $ newMVar True

mpgLoop = do
    let sh = "mpg123 -R"
    (Just hin, Just hout, _, hdl) <- createProcess (shell sh){ std_in = CreatePipe, std_out=CreatePipe }
    hPutStrLn hin "SILENCE"
    putMVar mhin hin
    putMVar mhout hout
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
handleKey 'q' = do
    send "STOP"
    shutdown
handleKey 'h' = do
    let msg = [
            " -= terminal control keys =-",
            "[p] or [ ] play/pause",
            "[n]    next track",
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
    hin <- readMVar mhin
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

module Douban.Player where

import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process
import Control.Concurrent

mhin = unsafePerformIO $ newEmptyMVar
mhout = unsafePerformIO $ newEmptyMVar

mpgLoop = do
    let sh = "mpg123 -R"
    (Just hin, Just hout, _, hdl) <- createProcess (shell sh){ std_in = CreatePipe, std_out=CreatePipe }
    hPutStrLn hin "SILENCE"
    putMVar mhin hin
    putMVar mhout hout
    print hin
    waitForProcess hdl
    hPutStrLn hin "STOP"
    hGetContents hout
    hFlush hin
    return ()

getKey = do
    k <- getChar
    handleKey k
    getKey

handleKey 'p' = do
    hin <- readMVar mhin
    hPutStrLn hin "PAUSE"
    hFlush hin
handleKey 's' = do
    hin <- readMVar mhin
    hPutStrLn hin "STOP"
    hFlush hin
handleKey _ = getKey

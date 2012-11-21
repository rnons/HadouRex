module Douban.Utils where

import System.Exit
import System.IO
import System.Posix.Process

showHelp arg = do
    msg <- readFile "commands"
    putStrLn msg
    return ()

shutdown = do
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    exitImmediately ExitSuccess
    

module Douban.Utils where

import System.Exit
import System.IO
import System.Posix.Process

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
    

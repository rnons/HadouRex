module Douban.Util where

import System.Exit
import System.IO
import System.Posix.Process

shutdown = do
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    exitImmediately ExitSuccess
    

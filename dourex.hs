import System.Environment
import Control.Concurrent
import GHC.IO.Exception
import Douban.FM
import Douban.Search
import Douban.Player
import System.IO

main = do
    --(command:argList) <- getArgs
    --dispatch command argList
    forkIO mpgLoop
    forkIO mpgInput
    --mpgInput >>= forkIO
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    getKey

mpgInput = do
    command <- getArgs
    if command /= [] then do
        (command:argList) <- getArgs
        dispatch command argList
        else select

--dispatch :: String -> [String] -> IO GHC.IO.Exception.ExitCode
dispatch :: String -> [String] -> IO ()
dispatch "search" = search
dispatch "hot" = hot
dispatch "trending" = trending
--dispatch "login" = login
--dispatch "" = select
--dispatch ("listen":[]) = select
dispatch "listen" = listen
dispatch x = help

help arg = do
    msg <- readFile "commands"
    putStrLn msg
    --TODO: hide "ExitSuccess" msg on the last line
    --return GHC.IO.Exception.ExitSuccess
    return ()

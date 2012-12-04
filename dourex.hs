import System.Environment
import Control.Concurrent
import GHC.IO.Exception
import Douban.FM
import Douban.Search
import Douban.Player
import Douban.Utils
import System.IO

main = do
    forkIO mpgLoop
    mpgInput
    --forkIO mpgInput
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

dispatch :: String -> [String] -> IO ()
dispatch "search" = search
dispatch "hot" = hot
dispatch "trending" = trending
dispatch "mark" = mark
dispatch "unmark" = unmark
dispatch "marks" = marks
dispatch "login" = login
dispatch "listen" = listen
dispatch x = showHelp


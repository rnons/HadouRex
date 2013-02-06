import System.Environment               (getArgs)
import Control.Concurrent               (forkIO)
import Douban.FM.Player.HadouRex
import Douban.FM.Player.HadouRex.Utils
import System.IO

main :: IO ()
main = do
    forkIO mpgInit
    parseArgs
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    getKey

parseArgs :: IO ()
parseArgs = do
    command <- getArgs
    if command /= [] then do
        (command:argList) <- getArgs
        dispatch command argList
        else select

dispatch :: String -> [String] -> IO ()
dispatch "listen" = listen
dispatch "search" = search
dispatch "hot" = hot
dispatch "trending" = trending
dispatch "marks" = marks
dispatch "mark" = mark
dispatch "unmark" = unmark
dispatch x = showHelp

